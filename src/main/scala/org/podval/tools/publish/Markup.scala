package org.podval.tools.publish

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

  final def dropAnchors(element: Xml.Element): Xml.Element = Xml.transform(
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
            .getOrElse(throw IllegalArgumentException(s"Defect: No id or title on $element"))
        )
  )

  final def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element = Xml.transform(
    element,
    delve = resolveLinks,
    transformText = xml =>
      if resolveWikiLinks
      then WikiLink.resolveWikiLinks(Seq.empty, Xml.atomText(xml), page)
      else Seq(xml),
    transformElement = element => linkElement(element) match
      case None => element
      case Some(linkElement) => page.site.resolveLink(Link(
        page = page,
        element = Some(element),
        ref = linkElement.ref,
        text = linkElement.text,
        kind = linkElement.kind,
        context = None,
        transclude = false
      ))
  )

  final def addToc(element: Xml.Element, toc: Toc): Xml.Element = Xml.transform(
    element,
    delve = resolveLinks,
    transformElement = element =>
      if !Toc.isKramdownTocMarker(element)
      then element
      else toc.toXml
  )
