package org.podval.tools.publish.pages

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{FrontMatter, MarkupPage, MarkupSource, Path, Site}
import org.podval.xml.Html
import zio.blocks.html.*

object ErrorsReport:
  object Maker extends MarkupPage.AutoMaker[ErrorsReport](Path("errors").html, ErrorsReport.apply)

final class ErrorsReport(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  source: Option[MarkupSource]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  source,
):
  override protected def titleDefault: String = "Errors"
  override protected def descriptionDefault: Option[String] = Some("Site errors by kind")
  override protected def iconDefault: Icon = Icon.errors
  override protected def headerPagePriorityDefault: Int = 9
  override protected def langDefault: Option[String] = Some("en")

  override def isSynthetic: Boolean = true
  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "site-errors", id := "site-errors")
