package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final class PageMarkup(
  val sourcePath: Path,
  markup: Markup,
  private var xml: Xml.Element
):
  xml = markup.dropAnchors(xml)

  private val toc: Toc = Toc(
    sections = markup.sections(xml),
    blocks = markup.blocks(xml)
  )

  def resolveLinks(page: MarkupPage): Unit =
    xml = markup.resolveLinks(xml, page)

  def resolveFragment(fragment: String): Option[Toc.Link] =
    toc.resolveFragment(fragment)

  def xmlContent: Xml.Element =
    xml = markup.addToc(xml, toc)
    xml

