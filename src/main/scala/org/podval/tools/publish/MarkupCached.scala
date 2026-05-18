package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml}
import Fragment.{Block, Section}

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

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = Xml.transform(xml, markup.stop(Xml), element =>
      var result: Xml.Element = element
      result = markup.resolveInternalLinks(result, page, errorReporter)
      result
    )

    // Convert to HTML and post-process some more
    Html.transform(Html.fromXml(xmlResult), markup.stop(Html), element =>
      var result: Html.Element = element
      result = markup.embed(result)
      result = markup.restoreWikiLinks(result)
      result = markup.addToc(result, sections)
      result
    )

  private lazy val ids: Seq[String] = Xml.gather(xml, markup.stop(Xml), Xml.Id.get)
  
  def resolveId(id: String): Option[Link.ToId] = ids.find(_ == id).map(Link.ToId(_))

  private lazy val blocks: Seq[Block] =
    if !markup.recognizeBlocks
    then Seq.empty
    else markup.getBlocks(xml, errorReporter)

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks.find(_.id == id).map(Link.ToBlock(_))

  private lazy val sections: Seq[Section] = markup.getSections(xml, errorReporter)
  
  def resolveSection(names: Seq[String]): Option[Link.ToSection] = Section
    .resolve(
      result = Seq.empty,
      sections = sections,
      names = names,
      includeNested = true
    )
    .map(Link.ToSection(_))

  def backLinks(page: MarkupPage): Seq[BackLinks.BackLink] = Xml.gatherWithParents(
    element = xml,
    stop = markup.stop(Xml),
    gatherElement = markup.backLink(_, _, page)
  )
