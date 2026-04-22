package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import java.net.{URI, URISyntaxException}
import XmlUtil.{a, apply, childrenWhenEmpty, href, replaceAttribute}

final class Links(pages: List[Page], warnings: Warnings):
  private var links: List[Link] = List.empty

  // TODO use for nav items
  def resolve(ref: String): Option[LinkResolved] = LinkResolved(pages, ref)
  
  // TODO one backlink per page
  def backLinks(page: Page): List[Link] = links.filter(_.toPage == page).filterNot(_.from.page == page)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  // TODO do the warning.recover/sequence dance!
  // TODO sort the pages in transclusion order and transclude
  def resolve(): Unit = pages.foreach(_.resolveLinks(this))

  // TODO mark errors with class attribute
  def resolve(from: Link.From): Xml.Element = from.fromElement.ref match
    case None => from.element.getOrElse(a("wiki-link", "/")()).childrenWhenEmpty(from.fromElement.text)
    case Some(ref) => (if !from.transclude then None else embed(ref, from.fromElement.text)).getOrElse:
      external(ref).fold(resolve(ref))(_ => None) match
        case None => from.element.getOrElse(a("wiki-link", ref)()).childrenWhenEmpty(from.fromElement.text)
        case Some(linkResolved) =>
          // Register resolved link
          links = links.appended(Link(from, linkResolved.page))

          if from.transclude then a("transclude", linkResolved.url)(linkResolved.text) else
            val result: Xml.Element = from.element match
              case None => a("wiki-link", linkResolved.url)()
              case Some(element) => element.copy(name = a).replaceAttribute(href, XmlUtil.escapeUrl(linkResolved.url))

            result.childrenWhenEmpty(Some(linkResolved.text))

  // see https://obsidian.md/help/embeds
  private def embed(ref: String, text: Option[String]): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.flatMap: extension =>
      if Files.imageExtensions.contains(extension) then None
      // Embedd image
      //      else if Files.audioExtensions.contains(extension) then
      //        // Embed audio player
      //      else if extension == "pdf" then
      //        // Embed pdf viewer
      else None

  private def external(ref: String): Option[URI] =
    if ref.contains(" ") then None else
      try
        val uri: URI = URI(ref)
        // TODO recognize and resolve links to *this* site
        Option.when(uri.getScheme != null)(uri)
      catch
        case e: URISyntaxException =>
          // TODO handle errors better - and log them
          println(s"Malformed URL: ${ref} $e")
          None
