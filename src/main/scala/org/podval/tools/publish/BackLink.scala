package org.podval.tools.publish

final class BackLink(
  val to: Page.Link,
  val from: MarkupPage,
  val wikiLink: Boolean,
  val transclude: Boolean,
  val kind: Option[String],
  val context: Option[String]
)
