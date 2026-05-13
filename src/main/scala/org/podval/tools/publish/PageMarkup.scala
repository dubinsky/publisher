package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.html.*
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec
import Page.{Block, Section}

object PageMarkup:
  private def isKramdownTocMarker(element: Html.Element): Boolean =
    Html.qName(element) == "ul" &&
    Html.children(element).length == 1 &&
    Html.asElement(Html.children(element).head).fold(false): child =>
      Html.qName(child) == "li" &&
      Html.children(child).length == 1 &&
      Html.asText(Html.children(child).head).fold(false): text =>
        text.endsWith("{:toc}")

final class PageMarkup(
  source: MarkupPage.Source,
  private var xml: Xml.Element
):
  private def markup: Markup = source.markup
  private def site: Site = source.site
  private def sourcePath: Path = source.sourcePath

  private var generatedId: Int = 0
  private def generateId: String =
    generatedId = generatedId + 1
    s"_generated_id$generatedId"
  
  // Pre-process raw XML
  this.xml = Xml.transform(xml, markup.stop(Xml), element =>
    var result: Xml.Element = element
    result = setSectionId(result)
    result = setBlockId(result)
    result = markup.convertLinks(result)
    result = convertWikiLinks(result)
    result = markInternalLinks(result)
    result
  )

  def htmlContent(page: Page): Html.Element =
    // Post-process XML
    val xmlResult: Xml.Element = Xml.transform(xml, markup.stop(Xml), element =>
      var result: Xml.Element = element
      result = resolveInternalLinks(result, page)
      result
    )

    // Convert to HTML and post-process some more
    Html.transform(Html.fromXml(xmlResult), markup.stop(Html), element =>
      var result: Html.Element = element
      result = embed(result)
      result = restoreWikiLinks(result)
      result = addToc(result)
      result
    )

  def resolveId(id: String): Option[Link.ToId] = ids
    .find(_ == id)
    .map(Link.ToId(_))
    
  private lazy val ids: Seq[String] = Xml.gather(xml, markup.stop(Xml), Xml.Id.get)
  
  def resolveBlock(id: String): Option[Link.ToBlock] = blocks
    .find(_.id == id)
    .map(Link.ToBlock(_))

  private lazy val blocks: Seq[Block] = if !markup.recognizeBlocks then Seq.empty else
    Xml.gather(xml, markup.stop(Xml), element =>
      if !Xml.WikiBlockClass.has(element) then None else Xml.Id.get(element) match
        case None => PageError.NoId(sourcePath, s"Defect: No id on block $element").report(site)
        case Some(id) => Some(Block(id))
    )
    
  private lazy val sections: Seq[Section] = markup.getSections(xml, site, sourcePath)

  private lazy val sectionsFlat: Seq[Section] =
    def forSection(section: Section): Seq[Section] = section +: section.sections.flatMap(forSection)
    sections.flatMap(forSection)

  def resolveSection(names: Seq[String]): Option[Link.ToSection] =
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
      .map(Link.ToSection(_))

  // Note: for Markdown, this can be achieved by setting `HtmlRenderer.GENERATE_HEADER_ID`,
  // but I do it manually and uniformly for HTML, TEI etc.
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
              case None => Xml.WikiBlockClass.add(Xml.Id.set(result, id))

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
        result = Xml.WikiLinkClass.add(result)
        if transclude then result = Xml.TranscludeClass.add(result)
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
        else
          val result: Xml.Element = Xml.InternalLinkClass.add(element)
          if Xml.Id.get(result).isDefined 
          then result
          else Xml.Id.set(result, generateId)

  def backLinks(page: MarkupPage): Seq[BackLinks.BackLink] = Xml.gatherWithParents(xml, markup.stop(Xml), (element, parents) =>
    if !Xml.A.is(element) || !Xml.InternalLinkClass.has(element) then None else
      for
        ref <- Xml.Href.get(element)
        linkTo <- Link.resolve(ref, page)
      yield BackLinks.BackLink(
        to = linkTo,
        from = page,
        transclude = Xml.TranscludeClass.has(element),
        kind = Xml.LinkKindClassPrefix.get(element).headOption,
        context = context(element, parents)
      )
    )

  // Note: I can widen the context by going after grandparent etc. if it is too short - but Obsidian does not seem to do it...
  private def context(focus: Xml.Element, parents: Seq[Xml.Element]): Html.Element =
    var parent: Xml.Element = parents.head
    parent = Xml.setChildren(parent, Xml.children(parent).map(node => Xml.asElement(node).fold(node): element =>
      if element ne focus then element else Xml.ClassName.add(element, "backLink-focus")
    ))
    Html.transform(Html.fromXml(parent), markup.stop(Html), restoreWikiLinks)

  private def resolveInternalLinks(element: Xml.Element, page: Page): Xml.Element =
    if !Xml.A.is(element) || !Xml.InternalLinkClass.has(element) then element else
      Xml.Href.get(element).fold(element)(ref => Link.resolve(ref, page) match
      case None =>
        PageError.Unresolved(sourcePath, s"unresolved internal link ref='$ref': $element}'").report(site, element)
        val result = Xml.ClassName.add(element, "unresolved-link")
        result
      case Some(linkTo) =>
        // TODO transclude
        var result: Xml.Element = Xml.Href.set(element, linkTo.url)
        if Xml.children(element).isEmpty then result = Xml.setText(result, linkTo.title)
        result
      )

  // see https://obsidian.md/help/embeds
  // TODO FlexMark inlines image links for the ![]() references - but does not process image sizes...
  private def embed(element: Html.Element): Html.Element = if !Html.A.is(element) then element else
    Html.Href.get(element).fold(element): ref =>
      if !Html.TranscludeClass.has(element) then element else
        val embedded: Option[Html.Element] = Files.nameAndExtension(ref)._2.fold(None): extension =>
          if Files.imageExtensions.contains(extension) then
            val (widthOpt: Option[Int], heightOpt: Option[Int]) =
              // TODO Embed image, potentially with sizes WIDTHxHEIGHT or just WIDTH or nothing in the text
              (None, None)

            Some(img(
              src := ref,
              alt := s"Image: $ref",
              widthOpt.map(value => width := value),
              heightOpt.map(value => height := value)
            ))
          else if Files.audioExtensions.contains(extension) then
            Some(audio(controls := true, src := ref))
          else if extension == "pdf" then
            // TODO Embed PDF viewer, with potentially page=PAGE&height=HEIGHT or one or none in the text
            None
          else
            None

        embedded.getOrElse:
          // TODO can not transclude external links
          element

  // Unresolved wiki links do not have any text; restore the ref as their text;
  // also, surround the text with explicit [[...]] instead of adding them in CSS.
  private def restoreWikiLinks(element: Html.Element): Html.Element =
    // TODO if internal non-wiki links can be empty, use InternalLinkClass instead?
    if !Html.A.is(element) || !Html.WikiLinkClass.has(element) then element else
      val text = Html.toStringOpt(element).orElse(Html.Href.get(element)).getOrElse("EMPTY LINK")
      Html.setText(element, s"[[$text]]")

  private def addToc(element: Html.Element): Html.Element =
    if !PageMarkup.isKramdownTocMarker(element) then element else Minima.toc(sections)

