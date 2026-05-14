package org.podval.tools.publish

import org.podval.xml.{Xml, XmlAst, XmlParser}
import zio.blocks.chunk.Chunk
import scala.annotation.tailrec
import Page.Section

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
abstract class HtmlLike extends Markup:
  final override def recognizeWikiLinks: Boolean = true

  final override def recognizeBlocks: Boolean = true

  final override def stop(xml: XmlAst)(element: xml.Element): Boolean = xml.Code.is(element)

  final override def convertLinks(element: Xml.Element): Xml.Element = element

  final override def isSectionElement(element: Xml.Element): Boolean = headerLevel(element).isDefined

  final override def getSectionTitle(element: Xml.Element): Option[String] = Xml.toStringOpt(element)

  private def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = Xml.qName(element)
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None

  // Note: only sections on the top level are detected;
  // sections of levels lower than the level of the first section are not allowed.
  final override def getSections(element: Xml.Element, site: Site, sourcePath: Path): Seq[Section] =
    val sectionElements: Chunk[HtmlLike.Section] = Xml
      .children(element)
      .flatMap(node => Xml.asElement(node))
      .flatMap(element =>
        for
          level <- headerLevel(element)
          title <- Xml.toStringOpt(element)
          id <-
            val id = Xml.Id.get(element)
            if id.isEmpty then PageError.NoId(sourcePath, s"Defect: No id on section $element").report(site, ())
            id
        yield HtmlLike.Section(
          level = level,
          title = title,
          id = id
        )
      )

    getSections(sectionElements)

  private def getSections(sections: Chunk[HtmlLike.Section]): Seq[Section] =
    if sections.isEmpty
    then Seq.empty
    else getSections(Seq.empty, sections.head.level, sections)

  @tailrec
  private def getSections(result: Seq[Page.Section], level: Int, sections: Chunk[HtmlLike.Section]): Seq[Section] =
    if sections.isEmpty then result else
      val head: HtmlLike.Section = sections.head
      val (nested: Chunk[HtmlLike.Section], tail: Chunk[HtmlLike.Section]) = sections.tail.span(_.level > head.level)
      val section: Page.Section = Page.Section(
        id = head.id,
        title = head.title,
        sections = getSections(nested)
      )

      getSections(
        result :+ section,
        level,
        tail
      )


object HtmlLike:
  private final class Section(
    val level: Int,
    val title: String,
    val id: String
  )

  object Html extends HtmlLike:
    override val extension: String = "html"
    override val additionalExtensions: Set[String] = Set.empty

    override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
      XmlParser.parse(content) match
        case Right(xml) => Right(Xml.asElement(xml).get)
        case Left(e) => Left(PageError.Parsing(sourcePath, "HTML parsing error", Some(e)))
