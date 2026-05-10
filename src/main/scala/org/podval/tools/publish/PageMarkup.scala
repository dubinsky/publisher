package org.podval.tools.publish

object PageMarkup:
  sealed abstract class PagePart(val id: String)

  final class Block(id: String) extends PagePart(id)

  object Block:
    val className: String = "wiki-block"

  final class Section(
    id: String,
    val title: String,
    val level: Int,
    val sections: Seq[Section]
  ) extends PagePart(id):
    def withSections(sections: Seq[Section]): Section = Section(id, title, level, sections)

  sealed abstract class Link:
    def title: String

    def id: String

  object Link:
    final class ToBlock(block: Block) extends Link:
      override def id: String = block.id
      override def title: String = s"^${block.id}"

    final class ToSection(sections: Seq[Section]) extends Link:
      override def id: String = sections.last.id
      override def title: String = sections.map(_.title).mkString("#")

final class PageMarkup(
  val markup: Markup,
  site: Site,
  val sourcePath: Path,
  private var xml: Xml.Element
):
  import PageMarkup.{Block, Section}

  xml = markup.process(xml, site, sourcePath)

  def resolveLinks(page: MarkupPage): Unit =
    xml = markup.resolveLinks(xml, page)

  def xmlContent: Xml.Element = xml

  def resolveFragment(fragment: String): Option[PageMarkup.Link] =
    if fragment.startsWith("^")
    then resolveBlock(id = fragment.substring(1).trim).map(PageMarkup.Link.ToBlock(_))
    else resolveSection(names = fragment.split('#').map(_.trim).toSeq).map(PageMarkup.Link.ToSection(_))

  lazy val sections: Seq[Section] = markup.getSections(xml, site, sourcePath)

  private lazy val blocks: Seq[Block] = markup.getBlocks(xml, site, sourcePath)

  private def resolveBlock(id: String): Option[Block] =
    blocks.find(_.id == id)

  private lazy val sectionsFlat: Seq[Section] =
    def forSection(section: Section): Seq[Section] = section +: section.sections.flatMap(forSection)
    sections.flatMap(forSection)

  private def resolveSection(names: Seq[String]): Option[Seq[Section]] =
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


