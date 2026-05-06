package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, a, div, el, ul, setId, withText}

final class Tags(
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
  private def slugify(text: String): String = text.replace(' ', '-')

  private def tagsAll: List[String] = site.markupPages.flatMap(_.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Xml.Element = a("page-tag", s"$path#${slugify(tag)}").withText(tag)

  override protected def syntheticContent: Option[Xml.Element] = Some:
    div("tags")(
      el("h2").withText("All tags"),
      el("p")(tagsAll.map(tagRef)*),
      el("h2").withText("Pages by tags"),
      el("ul")(tagsAll.map(tag =>
        el("li")(
          el("h3", "class" -> "page-tag").setId(slugify(tag)).withText(tag),
          ul("tag-pages-list", withTag(tag), _.ref("post-link"))
        )
      )*)
    )

object Tags:
  object Maker extends MarkupPage.AutoMaker[Tags](
    path = Path("tags").html,
    make = Tags.apply,
    frontMatterWithoutSource = FrontMatter(
      title = Some("Tags"),
      description = Some("Pages by tags"),
      //    permalink = Some(path.withoutExtension.toString)
    )
  )
