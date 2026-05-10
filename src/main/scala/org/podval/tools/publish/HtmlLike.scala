package org.podval.tools.publish

import scala.annotation.tailrec
import PageMarkup.Section

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
abstract class HtmlLike extends Markup:
  final override def recognizeWikiLinks: Boolean = true

  final override def recognizeBlocks: Boolean = true

  final override def stop(elementName: String): Boolean = elementName == Html.code

  final override def convertLinks(element: Xml.Element): Xml.Element = element

  final override def getSections(element: Xml.Element, site: Site, sourcePath: Path): Seq[Section] =
    nest(gather(element, element => getSection(element, site, sourcePath)))

  final override def isSectionElement(element: Xml.Element): Boolean = headerLevel(element).isDefined

  private def headerLevel(element: Xml.Element): Option[Int] =
    val qName: String = Xml.qName(element)
    if !qName.startsWith("h") then None else
      try Some(qName.substring(1).toInt)
      catch case _: NumberFormatException => None

  private def getSection(element: Xml.Element, site: Site, sourcePath: Path): Option[Section] =
    for
      level <- headerLevel(element)
      title <- Xml.toStringOpt(element)
      id <-
        val id = Xml.IdAttribute.get(element)
        if id.isEmpty then PageError.NoId(sourcePath, s"Defect: No id on section $element").report(site, ())
        id
    yield Section(
      sections = Seq.empty,
      level = level,
      title = title,
      id = id
    )

  private def nest(sections: Seq[Section]): Seq[Section] =
    @tailrec
    def loop(result: Seq[Section], sections: Seq[Section]): Seq[Section] =
      if sections.isEmpty then result else
        val section = sections.head
        val (deeper: Seq[Section], tail: Seq[Section]) = sections.tail.span(_.level > section.level)
        loop(result ++ Seq(section.withSections(nest(deeper))), tail)

    loop(Seq.empty, sections)

object HtmlLike:
  object Html extends HtmlLike:
    override val extension: String = "html"
    override val additionalExtensions: Set[String] = Set.empty

    override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
      // TODO move into Xml
      try Right(Xml.asElement(zio.blocks.schema.xml.XmlReader.read(content)))
      catch case e: zio.blocks.schema.xml.XmlCodecError => Left(PageError.Parsing(sourcePath, "", Some(e)))
