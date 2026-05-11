package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec
import scala.ref.SoftReference
import BackLinks.BackLink
import Page.{Block, Section}
import PageMarkup.Cached

final class PageMarkup(
  val markup: Markup,
  val site: Site,
  val sourcePath: Path
):
  private var cachedVar: Option[SoftReference[Cached]] = None
  private def cache(cached: Cached): Unit = cachedVar = Some(SoftReference(cached))

  def cache(xml: Xml.Element): Unit = cache(Cached(this, xml))

  private def cached: Cached = cachedVar.get.get match
    case Some(cached) => cached
    case None =>
      site.log.warn(s"Re-reading evicted PageMarkup: $sourcePath")
      val (frontMatter: FrontMatter, xml: Xml.Element) = site.parseMarkup(sourcePath, markup)
      val cached: Cached = Cached(this, xml)
      cached

  def resolveBlock(id: String): Option[Page.PartLink.ToBlock] = cached.resolveBlock(id)
  def resolveSection(names: Seq[String]): Option[Page.PartLink.ToSection] = cached.resolveSection(names)
  def backLinks(page: MarkupPage): Seq[BackLink] = cached.backLinks(page)
  def xmlContent: Xml.Element = cached.xmlContent
  def sections: Seq[Section] = cached.sections

object PageMarkup:
  private object InternalLinkClass extends Xml.ClassName("internal-link")
  private object WikiLinkClass extends Xml.ClassName("wiki-link")
  private object TranscludeClass extends Xml.ClassName("transclude")

  // TEI org/person/place, facsimile, etc.
  private object LinkKindClassPrefix extends Xml.ClassNamePrefix("ref-kind")

  private final class Cached(
    pageMarkup: PageMarkup,
    private var xml: Xml.Element
  ):
    private def markup: Markup = pageMarkup.markup
    private def site: Site = pageMarkup.site
    private def sourcePath: Path = pageMarkup.sourcePath

    this.xml = Xml.transform(xml, markup.stop, element =>
      var result: Xml.Element = element
      result = setSectionId(result)
      result = setBlockId(result)
      result = markup.convertLinks(result)
      result = convertWikiLinks(result)
      result = markInternalLinks(result)
      result = embed(result)
      result
    )

    def xmlContent: Xml.Element =
      xml = resolveLinks(xml)
      xml

    private lazy val blocks: Seq[Block] = if !markup.recognizeBlocks then Seq.empty else
      markup.gather(xml, element =>
        if !Xml.ClassName.has(element, Block.className) then None else Xml.Id.get(element) match
          case None => PageError.NoId(sourcePath, s"Defect: No id on block $element").report(site)
          case Some(id) => Some(Block(id))
      )

    def resolveBlock(id: String): Option[Page.PartLink.ToBlock] =
      blocks.find(_.id == id).map(Page.PartLink.ToBlock(_))

    lazy val sections: Seq[Section] = markup.getSections(xml, site, sourcePath)

    private lazy val sectionsFlat: Seq[Section] =
      def forSection(section: Section): Seq[Section] = section +: section.sections.flatMap(forSection)
      sections.flatMap(forSection)

    def resolveSection(names: Seq[String]): Option[Page.PartLink.ToSection] =
      def loop(result: Seq[Section], sections: Seq[Section], names: Seq[String]): Option[Seq[Section]] =
        if names.isEmpty then Some(result) else sections
          .find(section => section.title == names.head || section.id == names.head)
          .flatMap(section => loop(
            result = result :+ section,
            sections = section.sections,
            names = names.tail
          ))

      loop(
        result = Seq.empty,
        sections = sectionsFlat,
        names = names
      )
        .map(Page.PartLink.ToSection(_))

    private def setSectionId(element: Xml.Element): Xml.Element =
      if !markup.isSectionElement(element) || Xml.Id.get(element).isDefined then element else
        Xml.toStringOpt(element) match
          case None => PageError.NoId(sourcePath, s"No id or title on $element").report(site, element)
          case Some(title) => Xml.Id.set(element, Xml.Id.toId(title))

    // TODO according to the Obsidian documentation, block anchor can be added to a "structured block"
    // (e.g., a list) by putting it after the block, with empty lines before and after;
    // I'll deal with this later...
    private def setBlockId(element: Xml.Element): Xml.Element =
      if !markup.recognizeBlocks then element else
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
                  PageError.NoId(sourcePath, s"Block id '$id' conflicts with existing id '$idExisting'").report(site, result)
                case None => Xml.ClassName.add(Xml.Id.set(result, id), Block.className)

    private def convertWikiLinks(element: Xml.Element): Xml.Element =
      if !markup.recognizeWikiLinks then element else Xml.setChildren(element, Xml.children(element).flatMap(xml =>
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
          result = Xml.setAttribute(result, Html.Href.attributeName, ref.trim)
          result = PageMarkup.WikiLinkClass.add(result)
          if transclude then result = PageMarkup.TranscludeClass.add(result)
          textOpt.map(_.trim).filterNot(_.isEmpty).foreach(text => result = Xml.setText(result, text))
          result

        Some(
          Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(result),
          after
        )

    private def markInternalLinks(element: Xml.Element): Xml.Element =
      if !Xml.A.is(element) then element else
        Xml.Href.get(element).fold(element): ref =>
          val isInternal: Boolean = try
            val uri: URI = URI(ref)
            if uri.getScheme != null && uri.getHost == site.url
            then PageError.SelfLink(sourcePath, ref).report(site)
            uri.getScheme == null
          catch case e: URISyntaxException => true

          if !isInternal
          then
            // TODO verify that external link is not broken if the Site is so configured
            element
          else PageMarkup.InternalLinkClass.add(element)

    // see https://obsidian.md/help/embeds
    // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
    private def embed(element: Xml.Element): Xml.Element = if !Xml.A.is(element) then element else
      Xml.Href.get(element).fold(element): ref =>
        if !PageMarkup.TranscludeClass.has(element) then element else
          val embedded: Option[Xml.Element] = Files.nameAndExtension(ref)._2.fold(None): extension =>
            if Files.imageExtensions.contains(extension) then
              // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
              None
            else if Files.audioExtensions.contains(extension) then
              // TODO Embed audio player
              None
            else if extension == "pdf" then
              // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
              None
            else
              None

          embedded.getOrElse:
            // TODO can not transclude external links
            element

    def backLinks(page: MarkupPage): Seq[BackLink] = markup.gatherWithParents(xml, (element, parents) =>
      for
        ref <- internalLinkRef(element)
        linkTo <- site.resolveRef(ref)
      yield BackLink(
        to = linkTo,
        from = page,
        transclude = PageMarkup.TranscludeClass.has(element),
        kind = PageMarkup.LinkKindClassPrefix.get(element).headOption,
        context = context(parents)
      )
    )

    // TODO
    private def context(parents: Seq[Xml.Element]): Option[String] = None // TODO

    // TODO resolve 'img' too?
    private def resolveLinks(element: Xml.Element): Xml.Element = Xml.transform(element, markup.stop, element =>
      internalLinkRef(element).fold(element)(ref => site.resolveRef(ref) match
        case None =>
          PageError.Unresolved(sourcePath, s"unresolved internal link ref='$ref': $element}'").report(site, element)
          val result = Xml.ClassName.add(element, "unresolved-link")
          result
        case Some(linkTo) =>
          // TODO transclude
          var result: Xml.Element = Xml.Href.set(element, linkTo.url)
          if Xml.children(element).isEmpty then result = Xml.setText(result, linkTo.title)
          result
      ))

    private def internalLinkRef(element: Xml.Element): Option[String] =
      for
        ref <- Xml.Href.get(element)
        if Xml.A.is(element)
        if PageMarkup.InternalLinkClass.has(element)
      yield
        ref

