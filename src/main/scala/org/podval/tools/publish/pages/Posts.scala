package org.podval.tools.publish.pages

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{FrontMatter, MarkupPage, MarkupSource, Path, Site}
import org.podval.xml.Html
import zio.blocks.html.*

object Posts:
  object Maker extends MarkupPage.AutoMaker[Posts](Path("posts").html, Posts.apply)

final class Posts(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  source: Option[MarkupSource]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  source
):
  override protected def titleDefault: String = "Posts"
  override protected def descriptionDefault: Option[String] = Some("All posts")
  override protected def iconDefault: Icon = Icon.envelope
  override protected def headerPagePriorityDefault: Int = 1
  override protected def langDefault: Option[String] = Some("en")

  override def isSynthetic: Boolean = true
  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "home",
      //      h1(className := "page-heading", page.title)
      h2(className := "post-list-heading", "Posts"),
      ul(className := "post-list", site.markupPages.filter(_.isPost).sortBy(_.date).reverse.map(post =>
        li(
          span(className := "post-meta", post.date.map(_.toShortString).getOrElse("")),
          h3(className := "post-link", post.ref())
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%} // TODO unify with feed.xml
        )
      ))
    )
