package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final class PageMarkup(
  val sourcePath: Path,
  markup: Markup,
  private var xml: Xml.Element
):
  private lazy val toc: Toc =
    xml = markup.dropAnchors(xml)
    Toc(
      sections = markup.sections(xml),
      blocks = markup.blocks(xml)
    )

  def resolveLinks(page: MarkupPage): Unit =
    xml = markup.resolveLinks(xml, page)

  def xmlContent: Xml.Element =
    xml = markup.addToc(xml, toc)
    xml

  def resolveFragment(fragment: String): Option[Toc.Link] = toc.resolveFragment(fragment)
