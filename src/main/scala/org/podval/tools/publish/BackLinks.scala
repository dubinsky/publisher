package org.podval.tools.publish

import BackLinks.BackLink

final class BackLinks:
  // TODO I will have to serialize this to disc probably or something...
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def backLinks(page: Page): List[BackLink] = backLinks
    .filter(_.to.page == page)
    .filterNot(_.from == page)
    .distinctBy(_.from.path) // TODO once we have context, each link should be listed (grouped by page)

object BackLinks:
  final class BackLink(
    val to: Page.Link,
    val from: MarkupPage,
    val transclude: Boolean,
    val kind: Option[String],
    val context: Option[String]
  )
  