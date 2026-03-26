package org.podval.tools.publish

final class LinkResolver(linkResolver: Links, page: MarkupPage):
  def resolveUrl(
    url: String,
  ): Option[Page] = linkResolver.resolveUrl(
    page,
    url
  )

  def resolveName(
    name: String,
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String] // from.markup.AST?
  ): Option[Page] = linkResolver.resolveName(
    page,
    name,
    category,
    context
  )
 