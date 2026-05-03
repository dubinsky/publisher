package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final case class Link(
  from: Link.From,
  to: Page.Link
)

object Link:
  final case class From(
    page: MarkupPage,
    element: Option[Xml.Element], // None for wiki link
    ref: String,
    text: Option[String],
    kind: Option[String],
    context: Option[String],  // TODO retrieve link context
    // TODO section
    transclude: Boolean
  )
