package org.podval.tools.publish

import scala.annotation.tailrec
import Toc.Section
import Xml.getAttribute

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
abstract class HtmlLike extends Markup:
  final override def resolveLinks(element: Xml.Element): Boolean =
    Xml.qName(element) != Html.code

  final override def resolveWikiLinks: Boolean = true

  final override def sectionElement(element: Xml.Element): Option[Markup.SectionElement] =
    // Extract level of the HTML '<h>' element.
    val headerLevel: Option[Int] =
      if element.name.prefix.isDefined || !element.name.localName.startsWith("h") then None else
        try Some(element.name.localName.substring(1).toInt)
        catch case _: NumberFormatException => None
        
    headerLevel.map(level => Markup.SectionElement(
      level = Some(level),
      id = element.getAttribute(Xml.idAttr),
      text = Xml.toStringOpt(element)
    ))
  
  // TODO do 'img' too?
  final override def linkElement(element: Xml.Element): Option[Markup.LinkElement] =
    if Xml.qName(element) != Html.a then None else element
      .getAttribute(Html.hrefAttr)
      .map(ref => Markup.LinkElement(
        ref = ref,
        text = Xml.toStringOpt(element),
        kind = None
      ))

  final override def sections(xml: Xml.Element): Seq[Section] = nest(getSections(xml))

  final override def blocks(xml: Xml.Element): Seq[Toc.Block] = Seq.empty // TODO

  private def nest(sections: Seq[Section]): Seq[Section] =
    @tailrec
    def loop(result: Seq[Section], sections: Seq[Section]): Seq[Section] =
      if sections.isEmpty then result else
        val section = sections.head
        val (deeper: Seq[Section], tail: Seq[Section]) = sections.tail.span(_.level > section.level)
        loop(result ++ Seq(section.withSections(nest(deeper))), tail)

    loop(Seq.empty, sections)

  private def getSections(element: Xml.Element): Seq[Section] =
    if !resolveLinks(element) then Seq.empty else
      val section: Option[Section] = for
        sectionElement <- HtmlLike.Html.sectionElement(element)
        level <- sectionElement.level
        title <- sectionElement.text
      yield Section(
        sections = Seq.empty,
        level = level,
        title = title,
        id = sectionElement.id.getOrElse(throw IllegalArgumentException(s"Defect: No id on $element"))
      )

      section.toSeq ++ element.children.flatMap {
        case element: Xml.Element => getSections(element)
        case xml => Seq.empty
      }

object HtmlLike:
  object Html extends HtmlLike:
    override val extension: String = "html"
    override val additionalExtensions: Set[String] = Set.empty

    override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
      // TODO move into Xml
      try Right(Xml.asElement(zio.blocks.schema.xml.XmlReader.read(content)))
      catch case e: zio.blocks.schema.xml.XmlCodecError => Left(PageError.Parsing(sourcePath, "", Some(e)))
