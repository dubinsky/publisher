package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import java.net.{URI, URISyntaxException}

object Links:
  final case class LinkResolved(url: String, text: String)

final class Links(pages: List[Page], warnings: Warnings):
  import Links.LinkResolved

  private var links: List[Link] = List.empty

  private def addLink(link: Link): Unit = links = links.appended(link)

  def backLinks(page: Page): List[Link] = links.filter(_.toPage == page).filterNot(_.fromPage == page)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  def resolve(page: Page): Unit =
    val linkElementResolvers: Seq[LinkElementResolver] = page.markup.linkElementResolvers
    def loop(xml: Xml): Xml = xml match
      case Xml.Element(name, attributes, children) =>
        val resolved: Option[Xml.Element] = for
          linkElementResolver <- linkElementResolvers.find(_.elementName == name)
          url <- Html.getAttribute(attributes, linkElementResolver.urlAttributeName)
          linkResolved <- resolve(
            fromPage = page,
            url = url,
            category = linkElementResolver.category,
            context = None // TODO retrieve link context
          )
        yield Xml.Element(
          name,
          Html.replaceAttribute(attributes, linkElementResolver.urlAttributeName, Html.escapeUrl(linkResolved.url)),
          if !children.isEmpty then children else Chunk(Xml.Text(Html.escapeText(linkResolved.text)))
        )

        resolved.getOrElse(Xml.Element(name, attributes, children.map(loop)))

      case xml => xml

    page.xml = loop(page.xml)

  def resolve(
    fromPage: Page,
    url: String, // could be `name`, `name#section`, `name#^block`, `path/name` etc...
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String]
  ): Option[LinkResolved] =
    val external: Option[LinkResolved] =
      if url.contains(" ") then None else
        try
          val uri: URI = URI(url)
          // TODO recognize and resolve links to *this* site
          if uri.getScheme == null then None else
            // TODO be more gentle with the name returned ;)
            Some(LinkResolved(url, ""))
        catch
          case e: URISyntaxException =>
            // TODO handle errors better - and log them
            println(s"Malformed URL: $url $e")
            None

    external.orElse:
      val (toPath: String, fragment: Option[String]) = Files.split(url, '#')

      pages.find(_.targetPathString.endsWith(toPath)).flatMap: toPage =>
        fragment match
          case None =>
            addLink(Link(
              fromPage = fromPage,
              url = url,
              category = category,
              context = context,
              toPage = toPage
            ))
            Some(LinkResolved(toPage.targetPath.toString, toPage.title))

          case Some(fragment) =>
            // TODO
            None

