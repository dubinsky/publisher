package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, a, div, el, setId, withText}

// TODO include (some of the) SyntheticPages in the link resolution
// or introduce (non-transcludable) Link.ToAnchor?
final class Tags(
  site: Site,
  path: Path
) extends SyntheticPage(
  path = path,
  frontMatter = FrontMatter(
    title = Some("Tags"),
    layout = Some("page"),
    description = Some("Pages by tags"),
    //    permalink = Some(path.withoutExtension.toString)
  )
):
  private def slugify(text: String): String = text.replace(' ', '-')

  private def tags: List[String] = site.pages.flatMap(_.frontMatter.tags).distinct.sorted

  def tagRef(tag: String): Xml.Element = a("page-tag", s"$path#${slugify(tag)}").withText(tag)

  private def withTag(tag: String): List[PageBase] = site.pages.filter(_.frontMatter.tags.contains(tag)).sortBy(_.title)

  override def xml: Xml.Element =
    val tags: List[String] = this.tags

    div("tags")(
      el("h2").withText("All tags"),
      el("p")(tags.map(tagRef)*),
      el("h2").withText("Pages by tags"),
      el("ul")(tags.map(tag =>
        el("li")(
          el("h3", "class" -> "page-tag").setId(slugify(tag)).withText(tag),
          el("ul")(withTag(tag).map(page =>
            el("li")(page.ref)
          )*)
        )
      )*)
    )
