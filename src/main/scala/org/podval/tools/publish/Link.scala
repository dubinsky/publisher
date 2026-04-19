package org.podval.tools.publish

final case class Link(
  fromPage: Page,
  url: String,
  category: Option[String],
  context: Option[String],

  toPage: Page
)