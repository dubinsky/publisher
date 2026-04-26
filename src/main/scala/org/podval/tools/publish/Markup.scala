package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.replaceAttribute

object Markup:
  val all: List[Markup] = List(
    Markdown,
    Html
  )

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def resolveLinks(element: Xml.Element): Boolean

  def resolveWikiLinks: Boolean
  
  def sectionElement(element: Xml.Element): Option[Link.SectionElement]

  def linkFromElement(element: Xml.Element): Option[Link.FromElement]

  def sections(xml: Xml.Element): Seq[Section]

  final def dropAnchors(element: Xml.Element): Xml.Element =
    if !resolveLinks(element) then element else
      val result: Xml.Element = sectionElement(element) match
        case None => element
        case Some(sectionElement) =>
          if sectionElement.id.isDefined then element else element.replaceAttribute(
            XmlUtil.id,
            sectionElement
              .text
              .map(XmlUtil.toId)
              .getOrElse(throw IllegalArgumentException(s"Defect: No id or title on $element"))
          )
  
      result.copy(children = result.children.map {
        case element: Xml.Element => dropAnchors(element)
        case xml => xml
      })

  
