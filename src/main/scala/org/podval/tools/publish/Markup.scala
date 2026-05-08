package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import scala.annotation.tailrec

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

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def recognizeWikiLinks: Boolean

  def recognizeBlocks: Boolean

  def resolveLinksElement(element: Xml.Element): Boolean

  def sectionElement(element: Xml.Element): Option[Markup.SectionElement]

  def linkElement(element: Xml.Element): Option[Markup.LinkElement]

  def withXml(sourcePath: Path, xml: Xml.Element): WithXml

  abstract class WithXml(
    val sourcePath: Path,
    private var xml: Xml.Element
  ):
    private var tocVar: Option[Toc] = None
    final def toc: Toc = tocVar.get

    final def buildToc(site: Site): Unit =
      xml = setSectionIds(xml, site)

      if recognizeBlocks then
        xml = setBlockIds(xml, site)

      tocVar = Some(Toc(
        sections = getSections(xml, site),
        blocks =
          if !recognizeBlocks
          then Seq.empty
          else getBlocks(xml, site)
      ))

    final def resolveLinks(page: MarkupPage): Unit =
      xml = resolveLinks(xml, page)

    final def xmlContent: Xml.Element =
      xml = addToc(xml, toc)
      xml

    def getSections(xml: Xml.Element, site: Site): Seq[Toc.Section]

    private def setSectionIds(element: Xml.Element, site: Site): Xml.Element = Xml.transform(
      element,
      delve = resolveLinksElement,
      transformElement = element => sectionElement(element) match
        case None => element
        case Some(sectionElement) =>
          if sectionElement.id.isDefined then element else
            sectionElement.text.map(Xml.toId) match
              case None => site.reportError(PageError.NoId(sourcePath, s"Defect: No id or title on $element"), element)
              case Some(id) => Xml.setAttribute(element, Xml.idAttr, id)
    )

    private def setBlockIds(element: Xml.Element, site: Site): Xml.Element = Xml.transform(
      element,
      delve = resolveLinksElement,
      transformElement = element =>
        val children: Chunk[Xml.Xml] = Xml.children(element)
        if children.isEmpty || !Xml.isText(children.last) then element else
          val (before: String, id: Option[String]) = Strings.split(Xml.atomText(children.last), '^')
          id.fold(element): id =>
            if before.nonEmpty && !Character.isWhitespace(before.last) then element else
              val result: Xml.Element = Xml.setChildren(element, children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq)
              Xml.getAttribute(result, Xml.idAttr) match
                case Some(idExisting) => site.reportError(PageError.NoId(sourcePath, s"Block id '$id' conflicts with existing id '$idExisting'"), result)
                case None => Xml.ClassAttribute.add(Xml.setAttribute(result, Xml.idAttr, id), Toc.Block.className)
    )

    // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
    // (e.g., a list) by putting it after the block, with empty lines before and after;
    // I'll deal with this later...
    private def getBlocks(element: Xml.Element, site: Site): Seq[Toc.Block] = Xml.gather(
      element,
      delve = resolveLinksElement,
      gatherElement = element =>
        Option.when(Xml.ClassAttribute.has(element, Toc.Block.className)):
          Xml.getAttribute(element, Xml.idAttr) match
            case None => site.reportError(PageError.NoId(sourcePath, s"Defect: No id on block $element"), None)
            case Some(id) => Some(Toc.Block(id))
        .flatten
    )

    private def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element =
      def loop(element: Xml.Element): Xml.Element = if !resolveLinksElement(element) then element else
        val result: Xml.Element = Xml.setChildren(element, Xml.children(element).flatMap: xml =>
          if Xml.isElement(xml)
          then Seq(loop(Xml.asElement(xml)))
          else if Xml.isText(xml) then
            if recognizeWikiLinks
            then resolveWikiLinks(Seq.empty, Xml.atomText(xml), page)
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

    private def addToc(element: Xml.Element, toc: Toc): Xml.Element = Xml.transform(
      element,
      delve = resolveLinksElement,
      transformElement = element =>
        if !Toc.isKramdownTocMarker(element)
        then element
        else toc.toXml
    )
