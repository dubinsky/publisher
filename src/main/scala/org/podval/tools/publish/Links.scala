package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import java.net.{URI, URISyntaxException}
import scala.annotation.tailrec
import XmlUtil.{a, apply, childWhen, getAttribute}

final class Links(pages: List[Page], warnings: Warnings):
  private var links: List[Link] = List.empty

  // TODO one backlink per page
  def backLinks(page: Page): List[Link] = links.filter(_.toPage == page).filterNot(_.from.page == page)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  // We handle wiki links in all kinds of markup ;)
  def resolve(page: Page): Unit =
    def loop(xml: Xml): Seq[Xml] = xml match
      case element: Xml.Element if page.markup.doResolveLinks(element) => Seq(resolveElement(element))
      case Xml.Text(value) => resolveWikiLinks(value, page)
      case xml => Seq(xml)

    def resolveElement(element: Xml.Element): Xml.Element = page.markup.linkElementResolver(element) match
      case None => element.copy(children = element.children.flatMap(loop))
      // TODO We do not resolve links inside link elements - should we?
      // TODO Markdown links can be transcluded also: ![](); parse this!
      case Some(linkElementResolver) => resolveLinkElement(element, linkElementResolver, page, transclude = false)

    // Note: we assume that root element allows link resolution
    require(page.markup.doResolveLinks(page.xml))

    page.xml = resolveElement(page.xml)

  private def resolveWikiLinks(text: String, page: Page): Seq[Xml] =
    @tailrec
    def loop(result: Seq[Xml], text: String): Seq[Xml] =
      if text.isEmpty then result else
        resolveWikiLink(text, "![[", "]]", transclude = true, page)
          .orElse(resolveWikiLink(text, "[[", "]]", transclude = false, page)) match
            case None => result ++ Seq(Xml.Text(text))
            case Some(xml: Seq[Xml], after: String) => loop(result ++ xml, after)

    loop(Seq.empty, text)

  private def resolveWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean,
    page: Page
  ): Option[(Seq[Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Files.split(body, '|')

      val result: Xml.Element = resolveWikiLink(
        ref.trim,
        textOpt.map(_.trim),
        page,
        transclude
      )

      Some(
        Option.when(before.nonEmpty)(Xml.Text(before)).toSeq ++ Seq(result),
        after
      )

  // see https://obsidian.md/help/links
  // see https://obsidian.md/help/embeds
  // TODO Markdown links can also be embedding: ![250](https://.../Engelbart.jpg) is equivalent to [[Engelbart.jpg|250]]...
  // TODO wiki links can't have a full URL; I am not sure if they can have a path even...
  private def resolveWikiLink(
    ref: String,
    textOpt: Option[String],
    page: Page,
    transclude: Boolean
  ): Xml.Element =
    resolve(
      isWiki = true,
      from = Link.From(
        page = page,
        ref = ref,
        category = None,
        context = None
      )
    ) match
      case None =>
        XmlUtil.a("wiki-link", ref)
          .childWhen(textOpt.nonEmpty, Xml.Text(textOpt.get))
          .build
      case Some(linkResolved) =>
        if !transclude then
          XmlUtil.a("wiki-link", linkResolved.url)(linkResolved.text)
        else
          val (_, extension: Option[String]) = Files.nameAndExtension(ref)
          // TODO handle embedded images etc.
          ???

  private def resolveLinkElement(
    element: Xml.Element,
    linkElementResolver: Link.ElementResolver,
    page: Page,
    transclude: Boolean
  ): Xml.Element =
    val linkResolved: Option[Link.Resolved] =
      for
        ref <- element.getAttribute(linkElementResolver.refAttributeName)
        linkResolved <- resolve(
          isWiki = false,
          from = Link.From(
            page = page,
            ref = ref,
            category = linkElementResolver.category,
            context = None
          )
        )
      yield linkResolved

    linkResolved match
      case None => element
      case Some(linkResolved) =>
        // TODO inherit other attributes
        a(
          element.getAttribute(XmlUtil.`class`).getOrElse("link-element"),
          XmlUtil.escapeUrl(linkResolved.url)
        )((
          if element.children.nonEmpty
          then element.children
          else Chunk(Xml.Text(XmlUtil.escapeText(linkResolved.text)))
        )*)

  private def resolve(isWiki: Boolean, from: Link.From): Option[Link.Resolved] =
    val external: Option[Link.Resolved] =
      if from.ref.contains(" ") then None else
        try
          val uri: URI = URI(from.ref)
          // TODO recognize and resolve links to *this* site
          if uri.getScheme == null then None else None
            // TODO be more gentle with the name returned ;)
//            Some(Link.Resolved(from.url, ""))
        catch
          case e: URISyntaxException =>
            // TODO handle errors better - and log them
            println(s"Malformed URL: ${from.ref} $e")
            None

    external.orElse:
      val (toPath: String, fragment: Option[String]) = Files.split(from.ref, '#')

      pages.find(_.is(toPath)).flatMap: toPage =>
        val result: Option[Link.Resolved] = fragment match
          case None =>
            Some(Link.Resolved.ToPage(toPage))
          case Some(fragment) =>
            if fragment.startsWith("^") then
              // TODO block reference
              ???
            else
              val names: Seq[String] = fragment.split('#').map(_.trim).toSeq
              toPage.toc.resolveSection(names).map(Link.Resolved.ToSection(toPage, _))

        result.foreach(_ => links = links.appended(Link(from, toPage)))
        result