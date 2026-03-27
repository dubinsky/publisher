package org.podval.tools.publish

object LinksResolver:
  final case class Link(url: String, name: String)

final class LinksResolver(linkResolver: Links, page: MarkupPage):
  def resolve(
    url: String, // could be `name`, `name#section`, `name#^block`, `path/name` etc...
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String] // from.markup.AST?
  ): Option[LinksResolver.Link] = linkResolver.resolve(
    page,
    url,
    category,
    context
  )
