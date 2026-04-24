package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import XmlUtil.getAttribute
import scala.annotation.tailrec

// Common for markup formats whose XML representation is actually HTML:
// HTML itself, Markdown, and likely Re-Structured text and AsciiDoc;
// pure XML markup formats like TEI and DocBook are different.
abstract class HtmlLike extends Markup:
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  final override def resolveLinks(element: Xml.Element): Boolean =
    element.name != XmlUtil.code

  final override def resolveWikiLinks: Boolean = true

  final override def linkToElement(element: Xml.Element): Option[Link.ToElement] =
    XmlUtil.headerLevel(element).map(_ => Link.ToElement(
      element.getAttribute(XmlUtil.id),
      Option.when(element.children.nonEmpty)(XmlUtil.toSimpleString(element))
    ))

  // TODO do 'img' too?
  final override def linkFromElement(element: Xml.Element): Option[Link.FromElement] =
    Option.when(element.name == XmlUtil.a)(Link.FromElement(
      ref = element.getAttribute(XmlUtil.hrefAttribute),
      text = Option.when(element.children.nonEmpty)(XmlUtil.toSimpleString(element)),
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
      val section: Option[Section] = XmlUtil.headerLevel(element).map(level =>
        val linkToElement: Link.ToElement = Html.linkToElement(element).get
        val title: String = XmlUtil.toSimpleString(element)
        Section(
          sections = Seq.empty,
          level = level,
          title = linkToElement.title.getOrElse(""),
          id = linkToElement.id.getOrElse(throw IllegalArgumentException(s"Defect: No id on ${XmlUtil.toSimpleString(element)}"))
        )
      )

      section.toSeq ++ element.children.flatMap {
        case element: Xml.Element => getSections(element)
        case xml => Seq.empty
      }
