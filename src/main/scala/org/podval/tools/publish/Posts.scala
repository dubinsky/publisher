package org.podval.tools.publish

import zio.blocks.html.*

final class Posts(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup
) with MarkupPage.BaseLayout:
  override def iconDefault: FontAwesome.Icon = FontAwesome.envelope

  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "home",
      //      h1(className := "page-heading", page.title)
      h2(className := "post-list-heading", "Posts"),
      ul(className := "post-list", site.markupPages.filter(_.isPost).sortBy(_.date).reverse.map(post =>
        li(
          span(className := "post-meta", post.date.map(_.toShortString).getOrElse("")),
          h3(className := "post-link", post.ref("post-link"))
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%} // TODO unify with feed.xml
        )
      ))
    )

object Posts:
  object Maker extends MarkupPage.AutoMaker[Posts](
    path = Path("posts").html,
    make = Posts.apply,
    frontMatterDefault = FrontMatter(
      title = Some("Posts"),
      description = Some("All posts"),
      lang = Some("en"),
      //    permalink = Some(path.withoutExtension.toString)
      headerPage = Some(FrontMatter.HeaderPage(priority = Some(1)))
    )
  )
