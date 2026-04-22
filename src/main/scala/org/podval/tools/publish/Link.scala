package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final case class Link(
  from: Link.From,
  toPage: Page
)

object Link:
  final case class ToElement(
    id: Option[String],
    title: Option[String]
  )

  final case class From(
    page: Page,
    fromElement: FromElement,
    context: Option[String],  // TODO retrieve link context
    // TODO section
    element: Option[Xml.Element],
    transclude: Boolean
  ):
    def isWikiLink: Boolean = element.isEmpty

  // TODO rename Ref?
  final case class FromElement(
    ref: Option[String], // could be `name`, `path/name`(?), `name#section`, `name#section#subsection`, `name#^block`.
    text: Option[String],
    category: Option[String] // TEI org/person/place, facsimile, etc.
  )
