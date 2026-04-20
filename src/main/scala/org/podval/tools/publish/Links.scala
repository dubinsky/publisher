package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import java.net.{URI, URISyntaxException}

final class Links(pages: List[Page], warnings: Warnings):
  private var links: List[Link] = List.empty

  def backLinks(page: Page): List[Link] = links.filter(_.toPage == page).filterNot(_.from.page == page)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  def resolve(page: Page): Unit =
    val linkElementResolvers: Seq[LinkElementResolver] = page.markup.linkElementResolvers
    def loop(xml: Xml): Xml = xml match
      case Xml.Element(name, attributes, children) =>
        val resolved: Option[Xml.Element] = for
          linkElementResolver <- linkElementResolvers.find(_.elementName == name)
          url <- Html.getAttribute(attributes, linkElementResolver.urlAttributeName)
          linkResolved <- resolve(Link.From(
            page = page,
            url = url,
            category = linkElementResolver.category,
            context = None // TODO retrieve link context
          ))
        yield Xml.Element(
          name,
          Html.replaceAttribute(attributes, linkElementResolver.urlAttributeName, Html.escapeUrl(linkResolved.url)),
          if !children.isEmpty then children else Chunk(Xml.Text(Html.escapeText(linkResolved.text)))
        )

        resolved.getOrElse(Xml.Element(name, attributes, children.map(loop)))

      case xml => xml

    page.xml = loop(page.xml)

  private def resolve(from: Link.From): Option[Link.Resolved] =
    val external: Option[Link.Resolved] =
      if from.url.contains(" ") then None else
        try
          val uri: URI = URI(from.url)
          // TODO recognize and resolve links to *this* site
          if uri.getScheme == null then None else None 
            // TODO be more gentle with the name returned ;)
//            Some(Link.Resolved(from.url, ""))
        catch
          case e: URISyntaxException =>
            // TODO handle errors better - and log them
            println(s"Malformed URL: ${from.url} $e")
            None

    external.orElse:
      val (toPath: String, fragment: Option[String]) = Files.split(from.url, '#')

      pages.find(_.is(toPath)).flatMap: toPage =>
        val result: Option[Link.Resolved] = fragment match
          case None => Some(Link.Resolved(toPage, Seq.empty))
          // TODO block references and transclusion
          case Some(fragment) => toPage.toc.resolveFragment(fragment).map(Link.Resolved(toPage, _))

        result.foreach(_ => links = links.appended(Link(from, toPage)))
        result