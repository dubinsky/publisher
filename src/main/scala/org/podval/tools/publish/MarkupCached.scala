package org.podval.tools.publish

import org.podval.tools.publish.util.IdGenerator
import org.podval.xml.{Html, Xml}
import Page.{Block, Section}

final class MarkupCached(
  source: MarkupSource,
  private var xml: Xml.Element
):
  private def markup: Markup = source.markup

  private val idGenerator: IdGenerator = IdGenerator()

  // Pre-process raw XML
  this.xml = Xml.transform(xml, markup.stop(Xml), element =>
    var result: Xml.Element = element
    result = markup.setSectionId(result, source)
    result = markup.setBlockId(result, source)
    result = markup.convertLinks(result)
    result = markup.convertWikiLinks(result)
    result = markup.markInternalLinks(result, source, idGenerator)
    result
  )

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = Xml.transform(xml, markup.stop(Xml), element =>
      var result: Xml.Element = element
      result = markup.resolveInternalLinks(result, page, source)
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

  def resolveId(id: String): Option[Link.ToId] = ids
    .find(_ == id)
    .map(Link.ToId(_))

  private lazy val blocks: Seq[Block] = if !markup.recognizeBlocks then Seq.empty else
    markup.getBlocks(xml, source)

  def resolveBlock(id: String): Option[Link.ToBlock] = blocks
    .find(_.id == id)
    .map(Link.ToBlock(_))

  private lazy val sections: Seq[Section] = markup.getSections(xml, source.site, source.sourcePath)

  private lazy val sectionsFlat: Seq[Section] =
    def forSection(section: Section): Seq[Section] = section +: section.sections.flatMap(forSection)
    sections.flatMap(forSection)

  def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    def loop(result: Seq[Section], sections: Seq[Section], names: Seq[String]): Option[Seq[Section]] =
      if names.isEmpty then Some(result) else sections
        .find(section => section.title == names.head || section.id == names.head)
        .flatMap(section => loop(
          result = result :+ section,
          sections = section.sections,
          names = names.tail
        ))

    loop(
      result = Seq.empty,
      sections = sectionsFlat,
      names = names
    )
      .map(Link.ToSection(_))

  def backLinks(page: MarkupPage): Seq[BackLinks.BackLink] = Xml.gatherWithParents(
    element = xml,
    stop = markup.stop(Xml),
    gatherElement = markup.backLink(_, _, page)
  )
