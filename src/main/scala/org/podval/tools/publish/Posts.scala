package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.{apply, child, div, el, setId, withText}
import zio.blocks.schema.xml.{Xml, XmlBuilder}

final class Posts(
  site: Site,
  path: Path,
  pageMarkup: Option[PageMarkup],
  frontMatter: FrontMatter
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
      el("ul", "class" -> "post-list")(site.posts.map(post =>
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
  final class Maker(site: Site, path: Path) extends MarkupPage.AutoMaker[Posts](site, path):
    override def withSource(pageMarkup: PageMarkup, frontMatter: FrontMatter): Posts =
      Posts(site, path, Some(pageMarkup), frontMatter)

    override def withoutSource: Posts =
      Posts(site, path, None, FrontMatter(
        title = Some("Posts")
        //    permalink = Some(path.withoutExtension.toString)
      ))
