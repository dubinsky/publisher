package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, a, div, el, ul, setId, withText}

// TODO include (some of the) SyntheticPages in the link resolution
// or introduce (non-transcludable) Link.ToAnchor?
final class Tags(
  site: Site,
  path: Path,
  pageMarkup: Option[PageMarkup],
  frontMatter: FrontMatter
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup
):
  private def slugify(text: String): String = text.replace(' ', '-')

  private def tags: List[String] = site.markupPages.flatMap(_.frontMatter.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.frontMatter.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Xml.Element = a("page-tag", s"$path#${slugify(tag)}").withText(tag)

  override protected def syntheticContent(content: Option[Xml.Element]): Xml.Element =
    val tags: List[String] = this.tags

    // TODO incorporate content
    div("tags")(
      el("h2").withText("All tags"),
      el("p")(tags.map(tagRef)*),
      el("h2").withText("Pages by tags"),
      el("ul")(tags.map(tag =>
        el("li")(
          el("h3", "class" -> "page-tag").setId(slugify(tag)).withText(tag),
          ul("tag-pages-list", withTag(tag), _.ref("post-link"))
        )
      )*)
    )

object Tags:
  final class Maker(site: Site, path: Path) extends MarkupPage.AutoMaker[Tags](site, path):
    override def withSource(pageMarkup: PageMarkup, frontMatter: FrontMatter): Tags =
      Tags(site, path, Some(pageMarkup), frontMatter)

    override def withoutSource: Tags =
      Tags(site, path, None, FrontMatter(
        title = Some("Tags"),
        description = Some("Pages by tags"),
        //    permalink = Some(path.withoutExtension.toString)
      ))

