package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.{apply, div, el, span, ul, withText}
import zio.blocks.schema.xml.Xml

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
  override protected def syntheticContent: Option[Xml.Element] = Some:
    div("home")(
      //      el("h1", "class" -> "page-heading").withText(page.title)
      el("h2", "class" -> "post-list-heading").withText("Posts"),
      el("ul", "class" -> "post-list")(site.markupPages.filter(_.isPost).sortBy(_.date).reverse.map(post =>
        el("li")(
          span("post-meta").withText(post.date.map(_.toShortString).getOrElse("")),
          el("h3", "class" -> "post-link")(post.ref("post-link"))
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%}
        )
      )*)
    )

object Posts:
  object Maker extends MarkupPage.AutoMaker[Posts](
    path = Path("posts").html,
    make = Posts.apply,
    frontMatterWithoutSource = FrontMatter(
      title = Some("Posts"),
      description = Some("All posts"),
      //    permalink = Some(path.withoutExtension.toString)
    )
  )
