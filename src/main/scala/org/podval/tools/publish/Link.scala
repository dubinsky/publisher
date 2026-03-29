package org.podval.tools.publish

final case class Link(
  source: Page.MarkupPage,
  url: String,
  category: Option[String],
  context: Option[String],
  anchor: Option[String],

  target: Page
)