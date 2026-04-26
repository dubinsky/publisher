package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import XmlUtil.{el, getAttribute}
import scala.annotation.tailrec

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
abstract class HtmlLike extends Markup:
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  final override def resolveLinks(element: Xml.Element): Boolean =
    element.name != XmlUtil.code

  final override def resolveWikiLinks: Boolean = true

  final override def sectionElement(element: Xml.Element): Option[Link.SectionElement] =
    // Extract level of the HTML '<h>' element.
    val headerLevel: Option[Int] =
      if element.name.prefix.isDefined || !element.name.localName.startsWith("h") then None else
        try Some(element.name.localName.substring(1).toInt)
        catch case _: NumberFormatException => None
        
    headerLevel.map(level => Link.SectionElement(
      level = Some(level),
      id = element.getAttribute(XmlUtil.id),
      text = XmlUtil.toStringOpt(element)
    ))
  
  // TODO do 'img' too?
  final override def linkFromElement(element: Xml.Element): Option[Link.FromElement] =
    if element.name != XmlUtil.a then None else element
      .getAttribute(XmlUtil.hrefAttribute)
      .map(ref => Link.FromElement(
        ref = ref,
        text = XmlUtil.toStringOpt(element),
        kind = None
      ))

  final override def sections(xml: Xml.Element): Seq[Section] = nest(getSections(xml))

  private def nest(sections: Seq[Section]): Seq[Section] =
    @tailrec
    def loop(result: Seq[Section], sections: Seq[Section]): Seq[Section] =
      if sections.isEmpty then result else
        val section = sections.head
        val (deeper: Seq[Section], tail: Seq[Section]) = sections.tail.span(_.level > section.level)
        loop(result ++ Seq(section.copy(sections = nest(deeper))), tail)

    loop(Seq.empty, sections)

  private def getSections(element: Xml.Element): Seq[Section] =
    if !resolveLinks(element) then Seq.empty else
      val section: Option[Section] = for
        sectionElement <- Html.sectionElement(element)
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
