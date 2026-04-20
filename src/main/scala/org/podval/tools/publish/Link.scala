package org.podval.tools.publish

final case class Link(
  from: Link.From,
  toPage: Page
)

object Link:
  final case class From(
    page: Page,
    url: String, // could be `name`, `path/name`, `name#section`, `name#section#subsection`, `name#^block`...
    category: Option[String],  // TEI org/person/place, facsimile, etc.
    context: Option[String],
    // TODO section
  )

  final case class Resolved(
    page: Page,
    sections: Seq[Toc.Section]
  ):
    def url: String =
      val fragment: String = if sections.isEmpty then "" else s"#${sections.last.id}"
      s"${page.targetPath.toString}$fragment"

    def text: String =
      val fragment: String = if sections.isEmpty then "" else sections.map(_.title).mkString("#", "#", "")
      s"${page.title}$fragment"

