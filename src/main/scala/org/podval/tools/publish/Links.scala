package org.podval.tools.publish

final class Links(
  pages: List[Page]
):
  def resolveUrl(
    from: MarkupPage,
    url: String,
  ): Option[Page] = ???

  def resolveName(
    from: MarkupPage,
    name: String,
    category: Option[String], // TEI org/person/place, etc.
    context: Option[String] // from.markup.AST?
  ): Option[Page] =
    println(s"LINK: $from [[$name]]")
    None
