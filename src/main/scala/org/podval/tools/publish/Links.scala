package org.podval.tools.publish

final class Links(warnings: Warnings):
  // TODO collect references
  // TODO calculate backlinks

  def resolve(
    from: Page.MarkupPage,
    url: String, // could be `name`, `name#section`, `name#^block`, `path/name` etc...
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String] // from.markup.AST?
  ): Option[LinksResolver.Link] =
    // TODO recognize and resolve links to this site; ignore outside (http[s]:) links...
    println(s"--> [[$url]] $from")
    None
    