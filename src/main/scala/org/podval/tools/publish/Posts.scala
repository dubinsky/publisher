package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.{apply, child, div, el, setId, withText}
import zio.blocks.schema.xml.{Xml, XmlBuilder}

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
  override protected def syntheticContent(content: Option[Xml.Element]): Xml.Element =
    div("home").child(content)(
      //      el("h1", "class" -> "page-heading").withText(page.title)
      el("h2", "class" -> "post-list-heading").withText("Posts"),
      el("ul", "class" -> "post-list")(site.markupPages.filter(_.isPost).sortBy(_.date).reverse.map(post =>
        el("li")(
          el("span", "class" -> "post-meta").withText(post.date.map(_.toShortString).getOrElse("")),
          el("h3")(post.ref("post-link"))
          // {%- if site.minima.show_excerpts -%} {{ post.excerpt }} {%- endif -%}
        )
      )*),
      el("p", "class" -> "rss-subscribe")(
        XmlBuilder.text("subscribe"),
        el("a", "href" -> "/feed.xml").withText("via RSS")
      )
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
