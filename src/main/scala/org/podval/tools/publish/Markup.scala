package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, IdGenerator, Media, Strings}
import org.podval.xml.{Html, Xml, XmlAst}
import zio.blocks.chunk.Chunk
import zio.blocks.html.*
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec

object Markup:
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html
  )

  // TEI org/person/place, facsimile, etc.
  private object LinkKindClassPrefix extends Xml.ClassNamePrefix("ref-kind")

  private object InternalLinkClass extends Xml.ClassName("internal-link")
  private object WikiLinkClass extends Xml.ClassName("wiki-link")
  private object WikiLinkClassHtml extends Html.ClassName(WikiLinkClass.name)
  private object TranscludeClass extends Xml.ClassName("transclude")
  private object TranscludeClassHtml extends Html.ClassName(TranscludeClass.name)
  private object WikiBlockClass extends Xml.ClassName("wiki-block")

  private def isKramdownTocMarker(element: Html.Element): Boolean =
    Html.qName(element) == "ul" && Html.children(element).exists: node =>
      Html.asElement(node).fold(false): child =>
        Html.qName(child) == "li" &&
        Html.children(child).length == 1 &&
        Html.asText(Html.children(child).head).fold(false): text =>
          text.endsWith("{:toc}")

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def recognizeWikiLinks: Boolean

  def recognizeBlocks: Boolean

  // TODO XmlWriter should stop at the same elements!
  def stop(xml: XmlAst)(element: xml.Element): Boolean

  def isSectionElement(element: Xml.Element): Boolean

  def getSectionTitle(element: Xml.Element): Option[String]

  def getSections(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Section]

  // This is where TEI link elements like `persName` get converted into HTML `a` elements
  def convertLinks(element: Xml.Element): Xml.Element

  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
  final def setSectionId(element: Xml.Element, errorReporter: PageError.Reporter): Xml.Element =
    if !isSectionElement(element) || Xml.Id.get(element).isDefined then element else
      getSectionTitle(element) match
        case None => errorReporter.error(PageError.NoId, s"No id nor title on $element", element)
        case Some(title) => Xml.Id.set(element, Xml.Id.toId(title))

  // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
  // (e.g., a list) by putting it after the block, with empty lines before and after;
  // I'll deal with this later...
  final def setBlockId(element: Xml.Element, errorReporter: PageError.Reporter): Xml.Element =
    if !recognizeBlocks then element else
      val children: Chunk[Xml.Xml] = Xml.children(element)
      if children.isEmpty then element else Xml.asText(children.last).fold(element): text =>
        val (before: String, id: Option[String]) = Strings.split(text, '^')
        id.fold(element): id =>
          if before.nonEmpty && !Character.isWhitespace(before.last) then element else
            val result: Xml.Element = Xml.setChildren(element,
              children.init ++ Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq
            )
            Xml.Id.get(result) match
              case Some(idExisting) =>
                errorReporter.error(PageError.NoId, s"Block id '$id' conflicts with existing id '$idExisting'", result)
              case None => Markup.WikiBlockClass.add(Xml.Id.set(result, id))

  final def getBlocks(element: Xml.Element, errorReporter: PageError.Reporter): Seq[Fragment.Block] =
    Xml.gather(element, stop(Xml), element =>
      if !Markup.WikiBlockClass.has(element) then None else Xml.Id.get(element) match
        case None => errorReporter.error(PageError.NoId, s"Defect: No id on block $element", None)
        case Some(id) => Some(Fragment.Block(id))
    )

  final def convertWikiLinks(element: Xml.Element): Xml.Element =
    if !recognizeWikiLinks then element else Xml.setChildren(element, Xml.children(element).flatMap(xml =>
      Xml.asText(xml).fold(Seq(xml))(text => convertWikiLinks(Seq.empty, text)))
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
        var result: Xml.Element = Xml.element(Xml.A.elementName)
        result = Xml.Href.set(result, ref.trim)
        result = Markup.WikiLinkClass.add(result)
        if transclude then result = Markup.TranscludeClass.add(result)
        textOpt.map(_.trim).filterNot(_.isEmpty).foreach(text => result = Xml.setText(result, text))
        result

      Some(
        Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(result),
        after
      )

  final def markInternalLinks(
    element: Xml.Element,
    errorReporter: PageError.Reporter,
    siteUrl: String,
    idGenerator: IdGenerator
  ): Xml.Element =
    if !Xml.A.is(element) then element else
      Xml.Href.get(element).fold(element): ref =>
        val isInternal: Boolean = try
          val uri: URI = URI(ref)
          if uri.getScheme != null && uri.getHost == siteUrl
          then errorReporter.error(PageError.SelfLink, ref, None)
          uri.getScheme == null
        catch case e: URISyntaxException => true

        if !isInternal
        then
          // TODO verify that external link is not broken if the Site is so configured
          element
        else
          val result: Xml.Element = Markup.InternalLinkClass.add(element)
          if Xml.Id.get(result).isDefined
          then result
          else Xml.Id.set(result, idGenerator.generate())

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  final def embed(element: Html.Element): Html.Element = if !Html.A.is(element) then element else
    Html.Href.get(element).fold(element): ref =>
      if !Markup.TranscludeClassHtml.has(element) then element else
        val embedded: Option[Html.Element] = Files.nameAndExtension(ref)._2.fold(None): extension =>
          if Media.isImage(extension) then
            val (widthOpt: Option[Int], heightOpt: Option[Int]) =
              // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
              (None, None)

            Some(img(
              src := ref,
              alt := s"Image: $ref",
              widthOpt.map(value => width := value),
              heightOpt.map(value => height := value)
            ))
          else if Media.isAudio(extension) then
            Some(audio(controls := true, src := ref))
          else if extension == "pdf" then
            // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
            None
          else
            None

        embedded.getOrElse:
          // TODO can not transclude external links
          element

  // Note: Obsidian expands the context to the source level, which is good for searching - but doesn't look great
  // when there are non-wiki links in there;
  // I am going with just text, so the non-wiki links are not going to be visible...
  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  final def backLink(element: Xml.Element, parents: Seq[Xml.Element], from: MarkupPage): Option[BackLinks.BackLink] =
    if !Xml.A.is(element) || !Markup.InternalLinkClass.has(element) then None else
      for
        ref <- Xml.Href.get(element)
        to <- Link.resolve(ref, from)
      yield
        val toFrom: Link = Link(from, fragment = from.resolveId(Xml.Id.get(element).get), intrapage = false)
        val (before: Chunk[Xml.Xml], tail) = Xml.children(parents.head).span(Xml.asElement(_).fold(true)(_ ne element))
        val after: Chunk[Xml.Xml] = tail.tail

        BackLinks.BackLink(
          to = to,
          from = from,
          transclude = Markup.TranscludeClass.has(element),
          kind = Markup.LinkKindClassPrefix.get(element).headOption,
          context = BackLinks.Context(
            url = toFrom.url,
            before = childrenText(before, isBefore = true),
            element = elementText(element),
            after = childrenText(after, isBefore = false)
          )
        )

  private def elementText(element: Xml.Element): String =
    Html.toString(Html.transform(Html.fromXml(element), stop(Html), restoreWikiLinks))

  private def childrenText(children: Chunk[Xml.Xml], isBefore: Boolean): String =
    shortenContext(isBefore, elementText(Xml.setChildren(Xml.element("dummy"), children)))

  private val contextLengthHalf: Int = 60

  private def shortenContext(isBefore: Boolean, string: String): String =
    if string.length <= contextLengthHalf then string else if isBefore then
      val result = string.substring(string.length - contextLengthHalf)
      val prefix = /*if result.startsWith(" ") then "" else*/ "..."
      prefix + result.trim
    else
      val result = string.substring(0, contextLengthHalf)
      val suffix = /*if result.endsWith(" ") then "" else*/ "..."
      result.trim + suffix

  final def resolveInternalLinks(element: Xml.Element, page: Page, errorReporter: PageError.Reporter): Xml.Element =
    if !Xml.A.is(element) || !Markup.InternalLinkClass.has(element) then element else
      Xml.Href.get(element).fold(element)(ref => Link.resolve(ref, page) match
        case None =>
          errorReporter.error(PageError.Unresolved, s"unresolved internal link ref='$ref': $element}'", element)
          val result = Xml.ClassName.add(element, "unresolved-link")
          result
        case Some(linkTo) =>
          // TODO transclude
          var result: Xml.Element = Xml.Href.set(element, linkTo.url)
          if Xml.children(element).isEmpty then result = Xml.setText(result, linkTo.title)
          result
      )

  // Unresolved wiki links do not have any text; restore the ref as their text;
  // also, surround the text with explicit [[...]] instead of adding them in CSS.
  final def restoreWikiLinks(element: Html.Element): Html.Element =
    // TODO if internal non-wiki links can be empty, use InternalLinkClass instead?
    if !Html.A.is(element) || !Markup.WikiLinkClassHtml.has(element) then element else
      val text = Html.toStringOpt(element).orElse(Html.Href.get(element)).getOrElse("EMPTY LINK")
      Html.setText(element, s"[[$text]]")

  final def addToc(element: Html.Element, sections: Seq[Fragment.Section]): Html.Element =
    if !Markup.isKramdownTocMarker(element) then element else
      div(className := "toc",
        h3("Table of Contents"),
        tocSections(sections)
      )

  private def tocSections(sections: Seq[Fragment.Section]): Html.Element =
    ul(sections.map(section =>
      li(
        a(href := s"#${section.id}", section.title),
        Option.when(section.sections.nonEmpty)(tocSections(section.sections))
      )
    ))
