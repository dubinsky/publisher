package org.podval.tools.publish

import org.podval.tools.publish.Toc.Section
import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec
import XmlUtil.replaceAttribute

// TODO to implement transclusion, build *real* sections, with content!
// TODO add TOC to page as needed
final case class Toc(sections: Seq[Toc.Section]):
  def resolveSection(names: Seq[String]): Option[Seq[Toc.Section]] =
    def resolve(result: Seq[Section], sections: Seq[Section], names: Seq[String]): Option[Seq[Section]] =
      if names.isEmpty then Some(result) else find(names.head, sections)
        .flatMap(section => resolve(
          result = result :+ section,
          sections = section.sections,
          names = names.tail
        ))

    resolve(
      result = Seq.empty,
      sections = sectionsFlat,
      names = names
    )

  private val sectionsFlat: Seq[Section] = sections.flatMap(_.sectionsFlat)

  private def find(name: String, sections: Seq[Section]): Option[Section] = sections.find(_.is(name))

object Toc:
  final case class Section(
    title: String,
    level: Int,
    id: String,
    sections: Seq[Section]
  ):
    def is(name: String): Boolean = title == name || id == name

    def sectionsFlat: Seq[Section] = sections.flatMap(_.sectionsFlat)

  def apply(xml: Xml.Element): (Xml.Element, Toc) =
    val (result: Xml.Element, sections: Seq[Section]) = setAnchorsAndGetSectionsForElement(xml)
    (result, new Toc(nest(sections)))

  private def nest(sections: Seq[Section]): Seq[Section] = nest(Seq.empty, sections)

  @tailrec
  private def nest(result: Seq[Section], sections: Seq[Section]): Seq[Section] =
    if sections.isEmpty then result else
      val section = sections.head
      val (deeper: Seq[Section], tail: Seq[Section]) = sections.tail.span(_.level > section.level)
      nest(result ++ Seq(section.copy(sections = nest(deeper))), tail)

  private def setAnchorsAndGetSections(xml: Xml): (Xml, Seq[Section]) = xml match
    case element: Xml.Element => setAnchorsAndGetSectionsForElement(element)
    case xml => (xml, Seq.empty)

  private def setAnchorsAndGetSectionsForElement(element: Xml.Element): (Xml.Element, Seq[Section]) = 
    // Extract level of the HTML '<h>' element.
    val sectionLevel: Option[Int] = XmlUtil.headerLevel(element)

    val section: Option[Section] = sectionLevel.map(level =>
      val title: String = XmlUtil.toSimpleString(element)
      Section(
        sections = Seq.empty,
        level = level,
        title = title,
        id = XmlUtil.toId(title)
      )
    )

    val childrenWithSections: Chunk[(Xml, Seq[Section])] = element.children.map(setAnchorsAndGetSections)
    val result: Xml.Element = element.copy(children = childrenWithSections.map(_._1))

    (
      section.fold(result)(section => result.replaceAttribute(XmlUtil.id, section.id)),
      section.toSeq ++ childrenWithSections.flatMap(_._2)
    )
