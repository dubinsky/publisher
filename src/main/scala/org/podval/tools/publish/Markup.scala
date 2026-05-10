package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec

object Markup:
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html
  )

  private val wikiLinkClass: String = "wiki-link"
  private val transcludeClass: String = "transclude"
  private val refKindClassPrefix: String = "ref-kind-" // TEI org/person/place, facsimile, etc.

abstract class Markup derives CanEqual:
  import PageMarkup.{Block, Section}

  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def recognizeWikiLinks: Boolean

  def recognizeBlocks: Boolean

  def stop(elementName: String): Boolean

  // This is where TEI link elements like `persName` get converted into HTML `a` elements
  def convertLinks(element: Xml.Element): Xml.Element

  def isSectionElement(element: Xml.Element): Boolean

  def getSections(element: Xml.Element, site: Site, sourcePath: Path): Seq[Section]

  final def gather[A](
    element: Xml.Element,
    gatherElement: Xml.Element => Option[A]
  ): Seq[A] =
    def loop(element: Xml.Element): Seq[A] = if stop(Xml.qName(element)) then Seq.empty else
      gatherElement(element).toSeq ++ Xml.children(element)
        .filter(Xml.isElement)
        .map(Xml.asElement)
        .flatMap(loop)

    loop(element)

  final def process(xml: Xml.Element, site: Site, sourcePath: Path): Xml.Element = Xml.transform(
    xml,
    stop = element => stop(Xml.qName(element)),
    element =>
      var result: Xml.Element = element
      result = setSectionId(result, site, sourcePath)
      result = setBlockId(result, site, sourcePath)
      result = convertLinks(result)
      result = convertWikiLinks(result)
      result
    )

  private def setSectionId(element: Xml.Element, site: Site, sourcePath: Path): Xml.Element =
    if !isSectionElement(element) || Xml.IdAttribute.get(element).isDefined then element else
      Xml.toStringOpt(element) match
        case None => PageError.NoId(sourcePath, s"No id or title on $element").report(site, element)
        case Some(title) => Xml.IdAttribute.set(element, Xml.IdAttribute.toId(title))

  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  private def setBlockId(element: Xml.Element, site: Site, sourcePath: Path): Xml.Element =
    if !recognizeBlocks then element else
      val children: Chunk[Xml.Xml] = Xml.children(element)
      if children.isEmpty || !Xml.isText(children.last) then element else
        val (before: String, id: Option[String]) = Strings.split(Xml.atomText(children.last), '^')
        id.fold(element): id =>
          if before.nonEmpty && !Character.isWhitespace(before.last) then element else
            val result: Xml.Element = Xml.setChildren(element,
              children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq
            )
            Xml.IdAttribute.get(result) match
              case Some(idExisting) =>
                PageError.NoId(sourcePath, s"Block id '$id' conflicts with existing id '$idExisting'").report(site, result)
              case None => Xml.ClassAttribute.add(Xml.IdAttribute.set(result, id), Block.className)

  final def getBlocks(element: Xml.Element, site: Site, sourcePath: Path): Seq[Block] =
    if !recognizeBlocks then Seq.empty else gather(element, element =>
      Option.when(Xml.ClassAttribute.has(element, Block.className)):
        Xml.IdAttribute.get(element) match
          case None => PageError.NoId(sourcePath, s"Defect: No id on block $element").report(site)
          case Some(id) => Some(Block(id))
      .flatten
    )

  private def convertWikiLinks(element: Xml.Element): Xml.Element =
    if !recognizeWikiLinks then element else Xml.setChildren(element, Xml.children(element).flatMap(xml =>
      if !Xml.isText(xml)
      then Seq(xml)
      else convertWikiLinks(Seq.empty, Xml.atomText(xml)))
    )

  // see https://obsidian.md/help/links
  @tailrec
  private def convertWikiLinks(result: Seq[Xml.Xml], text: String): Seq[Xml.Xml] =
    if text.isEmpty then result else
      convertWikiLink(text, "![[", "]]", transclude = true)
        .orElse(convertWikiLink(text, "[[", "]]", transclude = false)) match
          case None => result ++ Seq(Xml.mkText(text))
          case Some(xml: Seq[Xml.Xml], after: String) => convertWikiLinks(result ++ xml, after)

  private def convertWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean
  ): Option[(Seq[Xml.Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Strings.split(body, '|')

      val result: Xml.Element =
        val a = Xml
          .element("a")
          .attr("class", s"${Markup.wikiLinkClass}${if transclude then Markup.transcludeClass else ""}")
          .attr(Html.hrefAttr, ref.trim)
        textOpt.map(_.trim).filterNot(_.isEmpty).fold(a)(text => a.child(Xml.mkText(text))).build

      Some(
        Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(result),
        after
      )

  // TODO resolve 'img' too?
  final def resolveLinks(element: Xml.Element, page: MarkupPage): Xml.Element =
    def loop(element: Xml.Element, parents: Seq[Xml.Element]): Xml.Element = if stop(Xml.qName(element)) then element else
      val parentsNew: Seq[Xml.Element] = element +: parents
      val result: Xml.Element = Xml.setChildren(element, Xml.children(element).map(xml =>
        if !Xml.isElement(xml)
        then xml
        else loop(Xml.asElement(xml), parentsNew)
      ))

      if Xml.qName(result) != Html.a
      then result
      else Xml.getAttribute(result, Html.hrefAttr).fold(result)(href => resolveLink(result, href, page, parents))

    loop(element, Seq.empty)

  private def resolveLink(
    element: Xml.Element,
    ref: String,
    page: MarkupPage,
    parents: Seq[Xml.Element]
  ): Xml.Element =
    val site: Site = page.site
    val classes: List[String] = Xml.ClassAttribute.getList(element)
    val wikiLink: Boolean = classes.contains(Markup.wikiLinkClass)
    val transclude: Boolean = classes.contains(Markup.transcludeClass)
    val kind: Option[String] = classes
      .find(_.startsWith(Markup.refKindClassPrefix))
      .map(_.substring(Markup.refKindClassPrefix.length))
    val text: Option[String] = Xml.toStringOpt(element)

    // TODO mark errors with class attribute
    // TODO can not transclude external links
    (if !transclude then None else embedLink(ref, text)).getOrElse:
      if externalRef(ref).isDefined then element
      else site.resolveRef(ref) match
        case None =>
          PageError.Unresolved(page.path, s"unresolved internal link ref='$ref' text='${text.getOrElse("")}'").report(site, element)
        case Some(linkTo) =>
          // Register resolved link
          site.addBackLink(BackLink(
            to = linkTo,
            from = page,
            wikiLink = wikiLink,
            transclude = transclude,
            kind = kind,
            context = context(parents)
          ))

          if transclude then element else
            val result: Xml.Element = Xml.setAttribute(element, Html.hrefAttr, linkTo.url)
            if text.nonEmpty
            then result
            else Xml.setChildren(result, Chunk(Xml.mkText(linkTo.title)))

  private def context(parents: Seq[Xml.Element]): Option[String] = None // TODO

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embedLink(ref: String, text: Option[String]): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.flatMap: extension =>
      if Files.imageExtensions.contains(extension) then None
      // Embed image
      //      else if Files.audioExtensions.contains(extension) then
      //        // Embed audio player
      //      else if extension == "pdf" then
      //        // Embed PDF viewer
      else None

  private def externalRef(ref: String): Option[URI] =
    try
      val uri: URI = URI(ref)
      // TODO recognize and resolve links to *this* site
      Option.when(uri.getScheme != null)(uri)
    catch case e: URISyntaxException => None