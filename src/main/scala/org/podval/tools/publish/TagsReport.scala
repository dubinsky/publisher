package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, a, div, el, setId, withText}

final class TagsReport(
  targetPath: Path,
  site: Site
) extends SyntheticPage(
  targetPathSynthetic = targetPath,
  frontMatter = FrontMatter(
    title = Some("Tags"),
    layout = Some("page"),
    description = Some("Pages by tags"),
    //    permalink = Some(targetPath.withoutExtension.toString)
  )
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override def xml: Xml.Element =
    val tags: List[String] = site.tags

    div("tags").setId("tags")
      .child(el("h2").withText("All tags"))
      .child(el("p")(tags.map(tag => a("page-tag", s"#${slugify(tag)}").withText(tag))*))
      .child(el("h2").withText("Pages by tags"))
      .children(tags.map(tag =>
        div("tag-pages").setId(slugify(tag))
          .child(el("h3").withText(tag))
          .children(site.withTag(tag).map(page =>
            el("ul")( // TODO move out
              el("p")(a("post-link", page.targetPath.toString).withText(page.title))
            )
          )*)
          .build
      )*)
      .build


