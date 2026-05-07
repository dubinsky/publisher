package org.podval.tools.publish

import scala.annotation.tailrec
import Xml.replaceAttribute

object Markup:
  final case class SectionElement(
    level: Option[Int],
    id: Option[String],
    text: Option[String]
  )

  final case class LinkElement(
    ref: String,
    text: Option[String],
    kind: Option[String] // TEI org/person/place, facsimile, etc.
  )
  
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html
  )

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def resolveLinks(element: Xml.Element): Boolean

  def resolveWikiLinks: Boolean
  
  def sectionElement(element: Xml.Element): Option[Markup.SectionElement]

  def linkElement(element: Xml.Element): Option[Markup.LinkElement]

  def sections(xml: Xml.Element): Seq[Toc.Section]

  def blocks(xml: Xml.Element): Seq[Toc.Block]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  final def dropAnchors(element: Xml.Element): Xml.Element =
    if !resolveLinks(element) then element else
      val result: Xml.Element = sectionElement(element) match
        case None => element
        case Some(sectionElement) =>
          if sectionElement.id.isDefined
          then element
          else element.replaceAttribute(
            Xml.idAttr,
            sectionElement
              .text
              .map(Xml.toId)
              .getOrElse(throw IllegalArgumentException(s"Defect: No id or title on $element"))
          )
  
      result.copy(children = result.children.map {
        case element: Xml.Element => dropAnchors(element)
        case xml => xml
      })


  // TODO transform
  final def addToc(element: Xml.Element, toc: Toc): Xml.Element =
    if !resolveLinks(element) then element else
      element.copy(children = element.children.map {
        case element: Xml.Element if Toc.isKramdownTocMarker(element) => toc.toXml
        case element: Xml.Element => addToc(element, toc)
        case xml => xml
      })

  final def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element =
    if !resolveLinks(element) then element else
      val result: Xml.Element = element.copy(children = element.children.flatMap: xml =>
        if Xml.isElement(xml) then Seq(resolveLinks(Xml.asElement(xml), page))
        else if Xml.isAtom(xml) && resolveWikiLinks then resolveWikiLinks(Seq.empty, Xml.atomText(xml), page)
        else Seq(xml)
      )
      linkElement(result) match
        case None => result
        case Some(linkElement) => page.site.resolveLink(Link(
          page = page,
          element = Some(result),
          ref = linkElement.ref,
          text = linkElement.text,
          kind = linkElement.kind,
          context = None,
          transclude = false
        ))

  // see https://obsidian.md/help/links
  @tailrec
  private def resolveWikiLinks(result: Seq[Xml.Xml], text: String, page: MarkupPage): Seq[Xml.Xml] =
    if text.isEmpty then result else
      resolveWikiLink(text, "![[", "]]", transclude = true, page)
        .orElse(resolveWikiLink(text, "[[", "]]", transclude = false, page)) match
        case None => result ++ Seq(Xml.mkText(text))
        case Some(xml: Seq[Xml.Xml], after: String) => resolveWikiLinks(result ++ xml, after, page)

  private def resolveWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean,
    page: MarkupPage
  ): Option[(Seq[Xml.Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Strings.split(body, '|')

      val result: Xml.Element = page.site.resolveLink(Link(
        page = page,
        element = None,
        ref = ref.trim,
        text = textOpt.map(_.trim),
        kind = None,
        context = None,
        transclude = transclude
      ))

      Some(
        Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(result),
        after
      )
    