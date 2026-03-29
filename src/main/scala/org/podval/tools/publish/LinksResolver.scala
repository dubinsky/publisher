package org.podval.tools.publish

final class LinksResolver(linkResolver: Links, page: Page.MarkupPage):
  def resolve(
    url: String, // could be `name`, `name#section`, `name#^block`, `path/name` etc...
    category: Option[String], // TEI org/person/place, facsimile, etc.
    context: Option[String], // from.markup.AST?
    anchor: Option[String]
  ): Option[LinkResolved] = linkResolver.resolve(
    page,
    url,
    category,
    context,
    anchor
  )
