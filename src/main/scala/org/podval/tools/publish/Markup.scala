package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec
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


  final def addToc(element: Xml.Element, toc: Toc): Xml.Element =
    if !resolveLinks(element) then element else
      element.copy(children = element.children.map {
        case element: Xml.Element if Toc.isKramdownTocMarker(element) => toc.toXml
        case element: Xml.Element => addToc(element, toc)
        case xml => xml
      })

  final def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element =
    if !resolveLinks(element) then element else
      val result: Xml.Element = element.copy(children = element.children.flatMap {
        case element: Xml.Element => Seq(resolveLinks(element, page))
        case Xml.Text(text) if resolveWikiLinks => resolveWikiLinks(Seq.empty, text, page)
        case xml => Seq(xml)
      })
      linkFromElement(result) match
        case None => result
        case Some(linkFromElement) => page.site.resolveLink(Link.From(
          page = page,
          fromElement = linkFromElement,
          context = None,
          element = Some(element),
          transclude = false
        ))

  // see https://obsidian.md/help/links
  @tailrec
  private def resolveWikiLinks(result: Seq[Xml], text: String, page: MarkupPage): Seq[Xml] =
    if text.isEmpty then result else
      resolveWikiLink(text, "![[", "]]", transclude = true, page)
        .orElse(resolveWikiLink(text, "[[", "]]", transclude = false, page)) match
        case None => result ++ Seq(Xml.Text(text))
        case Some(xml: Seq[Xml], after: String) => resolveWikiLinks(result ++ xml, after, page)

  private def resolveWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean,
    page: MarkupPage
  ): Option[(Seq[Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Files.split(body, '|')

      val result: Xml.Element = page.site.resolveLink(Link.From(
        page = page,
        fromElement = Link.FromElement(
          ref = ref.trim,
          text = textOpt.map(_.trim),
          kind = None,
        ),
        context = None,
        element = None,
        transclude = transclude
      ))

      Some(
        Option.when(before.nonEmpty)(Xml.Text(before)).toSeq ++ Seq(result),
        after
      )
    