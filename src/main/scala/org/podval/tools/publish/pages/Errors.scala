package org.podval.tools.publish.pages

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{FrontMatter, MarkupPage, MarkupSource, Path, Site}
import org.podval.xml.Html
import zio.blocks.html.*

final class Errors(
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
  override def iconDefault: Icon = Icon.errors

  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "site-errors", id := "site-errors")

object Errors:
  object Maker extends MarkupPage.AutoMaker[Errors](
    path = Path("errors").html,
    make = Errors.apply,
    frontMatterDefault = FrontMatter(
      title = Some("Errors"),
      description = Some("Site errors by kind"),
      lang = Some("en"),
      //    permalink = Some(path.withoutExtension.toString)
      headerPage = Some(FrontMatter.HeaderPage(priority = Some(9)))
    )
  )
