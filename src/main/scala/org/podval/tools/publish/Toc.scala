package org.podval.tools.publish

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
    
  def toXml: Xml.Element = Xml
    .element("div")
    .attr("class", "toc")
    .child(Xml.element("h3").child(Xml.mkText("Table of Contents")).build)
    .children(toXml(sections))
    .build

  private def toXml(sections: Seq[Section]): Xml.Element = Xml
    .element("ul")
    .children(sections.map(section => Xml
      .element("li")
      .child(Xml.a("web-link", s"#${section.id}", section.title))
      .children(Option.when(section.sections.nonEmpty)(toXml(section.sections)).toSeq *)
      .build
    )*)
    .build

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

  def isKramdownTocMarker(element: Xml.Element): Boolean =
    element.name.localName == "ul" &&
    element.children.length == 1 &&
    Xml.isElement(element.children.head) && {
      val child = Xml.asElement(element.children.head)
      child.name.localName == "li" &&
      child.children.length == 1 &&
      Xml.isText(child.children.head) &&
      Xml.atomText(child.children.head).endsWith("{:toc}")
    }
