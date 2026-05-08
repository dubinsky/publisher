package org.podval.tools.publish

import zio.blocks.chunk.Chunk

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

  final def setSectionIds(element: Xml.Element, page: MarkupPage): Xml.Element = Xml.transform(
    element,
    delve = resolveLinks,
    transformElement = element => sectionElement(element) match
      case None => element
      case Some(sectionElement) =>
        if sectionElement.id.isDefined
        then element
        else Xml.setAttribute(
          element,
          Xml.idAttr,
          sectionElement
            .text
            .map(Xml.toId)
            // TODO page.site.reportError()
            .getOrElse(throw IllegalArgumentException(s"Defect: No id or title on $element"))
        )
  )

  final def setBlockIds(element: Xml.Element, page: MarkupPage): Xml.Element = Xml.transform(
    element,
    delve = resolveLinks,
    transformElement = element =>
      val children: Chunk[Xml.Xml] = Xml.children(element)
      if children.isEmpty || !Xml.isText(children.last) then element else
        val (before: String, id: Option[String]) = Strings.split(Xml.atomText(children.last), '^')
        id.fold(element): id =>
          if before.nonEmpty && !Character.isWhitespace(before.last) then element else
            val result: Xml.Element = Xml.setChildren(element, children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq)
            val idExisting: Option[String] = Xml.getAttribute(result, Xml.idAttr)
            // TODO page.site.reportError()
            if idExisting.nonEmpty then throw IllegalArgumentException(s"Block id '$id' conflicts with existing id '${idExisting.get}'")
            Xml.ClassAttribute.add(Xml.setAttribute(result, Xml.idAttr, id), Toc.Block.className)
  )

  final def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element =
    def loop(element: Xml.Element): Xml.Element = if !resolveLinks(element) then element else
      val result: Xml.Element = Xml.setChildren(element, Xml.children(element).flatMap: xml =>
        if Xml.isElement(xml)
        then Seq(loop(Xml.asElement(xml)))
        else if Xml.isText(xml) then
          if resolveWikiLinks
          then WikiLink.resolveWikiLinks(Seq.empty, Xml.atomText(xml), page)
          else Seq(xml)
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

    loop(element)

  final def addToc(element: Xml.Element, toc: Toc): Xml.Element = Xml.transform(
    element,
    delve = resolveLinks,
    transformElement = element =>
      if !Toc.isKramdownTocMarker(element)
      then element
      else toc.toXml
  )
