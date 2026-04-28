package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{a, apply, div, el, withText}

final class Toc(
  sections: Seq[Section]
):
  private val sectionsFlat: Seq[Section] = sections.flatMap(_.sectionsFlat)

  def section(names: Seq[String]): Option[Seq[Section]] =
    def loop(result: Seq[Section], sections: Seq[Section], names: Seq[String]): Option[Seq[Section]] =
      if names.isEmpty then Some(result) else sections.find(_.is(names.head)).flatMap(section => loop(
        result = result :+ section,
        sections = section.sections,
        names = names.tail
      ))

    loop(
      result = Seq.empty,
      sections = sectionsFlat,
      names = names
    )

  def block(id: String): Option[Block] = None // TODO
  
  def toXml: Xml.Element = div("toc")(toXml(sections))

  private def toXml(sections: Seq[Section]): Xml.Element =
    el("ul")(sections.map(section =>
      el("li")(toXml(section) *)
    ) *)

  private def toXml(section: Section): Seq[Xml.Element] =
    Seq(a("web-link", s"#${section.id}").withText(section.title)) ++
    Option.when(section.sections.nonEmpty)(toXml(section.sections)).toSeq

object Toc:
  def empty: Toc = Toc(Seq.empty)
  
  def isKramdownTocMarker(element: Xml.Element): Boolean =
    element.name.localName == "ul" &&
    element.children.length == 1 &&
    element.children.head.isInstanceOf[Xml.Element] &&
    element.children.head.asInstanceOf[Xml.Element].name.localName == "li" &&
    element.children.head.asInstanceOf[Xml.Element].children.length == 1 &&
    element.children.head.asInstanceOf[Xml.Element].children.head.isInstanceOf[Xml.Text] &&
    element.children.head.asInstanceOf[Xml.Element].children.head.asInstanceOf[Xml.Text].value.endsWith("{:toc}")
