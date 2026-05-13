package org.podval.tools.publish

import BackLinks.BackLink

final class BackLinks:
  private var backLinks: List[BackLink] = List.empty

  def addBackLinks(backLink: Seq[BackLink]): Unit = backLinks = backLinks.appendedAll(backLink)

  def backLinks(page: Page): Seq[(MarkupPage, List[BackLink])] = backLinks
    .filter(_.to.page == page)
    .filterNot(_.from == page)
    .groupBy(_.from)
    .toSeq
    .sortBy(_._1.title)

object BackLinks:
  final class Context(
    val url: String,
    val before: String,
    val it: String,
    val after: String
  )

  final class BackLink(
    val to: Link,
    val from: MarkupPage,
    val transclude: Boolean,
    val kind: Option[String],
    val context: Context
  )
