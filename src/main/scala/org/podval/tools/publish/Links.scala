package org.podval.tools.publish

import java.net.{URI, URISyntaxException}

final class Links(pages: List[Page], warnings: Warnings):
  private var links: List[Link] = List.empty

  def backLinks(page: Page): List[Link] = links.filter(_.target == page).filterNot(_.source == page)

  def resolve(
    page: Page.MarkupPage,
    url: String, // could be `name`, `name#section`, `name#^block`, `path/name` etc...
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String],
    anchor: Option[String]
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
      // TODO actually resolve
      // TODO add to links
      println(s"--> [[$url]] $page")
      None
