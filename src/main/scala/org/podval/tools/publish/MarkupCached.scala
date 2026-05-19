package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml}

final class MarkupCached(
  markup: Markup,
  errorReporter: PageError.Reporter,
  siteUrl: String,
  val frontMatter: FrontMatter,
  private var xml: Xml.Element
):
  private val idGenerator: IdGenerator = IdGenerator()

  // Pre-process raw XML
  this.xml = Xml.transform(xml, markup.stop(Xml), element =>
    var result: Xml.Element = element
    result = markup.setSectionId(result, errorReporter)
    result = markup.setBlockId(result, errorReporter)
    result = markup.convertLinks(result)
    result = markup.convertWikiLinks(result)
    result = markup.markInternalLinks(result, errorReporter, siteUrl, idGenerator)
    result
  )

  def backLinks(page: MarkupPage): Seq[BackLinks.BackLink] = Xml.gatherWithParents(
    element = xml,
    stop = markup.stop(Xml),
    gatherElement = markup.backLink(_, _, page)
  )

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = Xml.transform(xml, markup.stop(Xml), element =>
      var result: Xml.Element = element
      result = markup.resolveInternalLinks(result, page, errorReporter)
      result = markup.embed(result)
      result = markup.restoreWikiLinks(result)
      result
    )

    // Convert to HTML and add TOC
    Html.transform(Html.fromXml(xmlResult), markup.stop(Html), element =>
      if !Toc.isKramdownTocMarker(element) then element else toc.html
    )

  lazy val ids: Seq[String] = Xml.gather(xml, markup.stop(Xml), Xml.Id.get)

  lazy val blocks: Seq[Fragment.Block] =
    if !markup.recognizeBlocks
    then Seq.empty
    else markup.blocks(xml, errorReporter)

  lazy val toc: Toc = Toc(markup.sections(xml, errorReporter))
