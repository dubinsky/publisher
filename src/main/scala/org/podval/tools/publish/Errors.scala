package org.podval.tools.publish

import zio.blocks.html.*

final class Errors(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup,
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override protected def syntheticContent: Option[Html.Element] = Some:
    // TODO generate report page(s):
    //- broken links
    //- inconsistent titles
    div(className := "site-errors", id := "site-errors")

object Errors:
  object Maker extends MarkupPage.AutoMaker[Errors](
    path = Path("errors").html,
    make = Errors.apply,
    frontMatterDefault = FrontMatter(
      title = Some("Errors"),
      description = Some("Site errors by kind"),
      //    permalink = Some(path.withoutExtension.toString)
    )
  )
