package org.podval.tools.publish

import org.podval.tools.publish.Toc.Section
import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec

// TODO add TOC to page as needed
final case class Toc(sections: Seq[Toc.Section]):
  def resolveFragment(fragment: String): Option[Seq[Toc.Section]] =
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
      names = fragment.split('#').map(_.trim).toSeq
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

  def apply(xml: Xml): (Xml, Toc) =
    val (result: Xml, sections: Seq[Section]) = setAnchorsAndGetSections(xml: Xml)
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

  private def setAnchorsAndGetSectionsForElement(xml: Xml.Element): (Xml, Seq[Section]) = xml match
    case Xml.Element(name, attributes, children) =>
      // Extract level of the HTML '<h>' element.
      val sectionLevel: Option[Int] =
        if name.prefix.isDefined then None else
          if !name.localName.startsWith("h") then None else
            try Some(name.localName.substring(1).toInt)
            catch case _: NumberFormatException => None

      val section: Option[Section] = sectionLevel.map(level =>
        val title: String = Html.toSimpleString(xml)
        Section(
          sections = Seq.empty,
          level = level,
          title = title,
          id = Html.toId(title)
        )
      )

      val subs: Chunk[(Xml, Seq[Section])] = children.map(setAnchorsAndGetSections)

      val result: Xml.Element = Xml.Element(
        name = name,
        children = subs.map(_._1),
        attributes = section.map(section => Html.replaceAttribute(attributes, Html.id, section.id)).getOrElse(attributes)
      )

      (result, section.toSeq ++ subs.flatMap(_._2))
