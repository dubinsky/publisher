package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{a, apply, div, el, withText}
import Toc.{Block, Section}

final class Toc(
  sections: Seq[Section],
  blocks: Seq[Block]
):
  private val sectionsFlat: Seq[Section] =
    def forSection(section: Section): Seq[Section] = section +: section.sections.flatMap(forSection)
    sections.flatMap(forSection)

  def resolveFragment(fragment: String): Option[Toc.Link] =
    if fragment.startsWith("^")
    then block(id = fragment.substring(1).trim).map(Toc.Link.ToBlock(_))
    else section(names = fragment.split('#').map(_.trim).toSeq).map(Toc.Link.ToSection(_))

  private def block(id: String): Option[Block] =
    blocks.find(_.id == id)

  private def section(names: Seq[String]): Option[Seq[Section]] =
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
    
  def toXml: Xml.Element = div("toc")(toXml(sections))

  private def toXml(sections: Seq[Section]): Xml.Element =
    el("ul")(sections.map(section =>
      el("li")(toXml(section) *)
    ) *)

  private def toXml(section: Section): Seq[Xml.Element] =
    Seq(a("web-link", s"#${section.id}").withText(section.title)) ++
    Option.when(section.sections.nonEmpty)(toXml(section.sections)).toSeq

object Toc:
  sealed abstract class PagePart(val id: String)

  final class Block(id: String) extends PagePart(id)

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
  
  def empty: Toc = Toc(
    sections = Seq.empty,
    blocks = Seq.empty
  )
  
  def isKramdownTocMarker(element: Xml.Element): Boolean =
    element.name.localName == "ul" &&
    element.children.length == 1 &&
    element.children.head.isInstanceOf[Xml.Element] &&
    element.children.head.asInstanceOf[Xml.Element].name.localName == "li" &&
    element.children.head.asInstanceOf[Xml.Element].children.length == 1 &&
    element.children.head.asInstanceOf[Xml.Element].children.head.isInstanceOf[Xml.Text] &&
    element.children.head.asInstanceOf[Xml.Element].children.head.asInstanceOf[Xml.Text].value.endsWith("{:toc}")
