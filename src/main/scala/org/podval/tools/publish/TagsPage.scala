package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, a, div, el, setId}

final class TagsPage(
  targetPath: Path,
  site: Site
) extends SyntheticPage(
  targetPath,
  PageKind.Plain,
  FrontMatter(
    title = Some("Tags"),
    layout = Some("page"),
    description = Some("Pages by tags"),
//    permalink = Some(targetPath.withoutExtension.toString)
  )
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override def xml: Xml.Element =
    val tags: List[String] = site.pages.flatMap(_.frontMatter.tags).distinct.sorted
    def pagesWithTag(tag: String): List[Page] = site.pages.filter(_.frontMatter.tags.contains(tag)).sortBy(_.title)

    div("tags").setId("tags")
      .child(el("h2")("All tags"))
      .child(el("p")(tags.map(tag => a("page-tag", s"#${slugify(tag)}")(tag))*))
      .child(el("h2")("Pages by tags"))
      .children(tags.map(tag =>
        div("tag-pages").setId(slugify(tag))
          .child(el("h3")(tag))
          .children(pagesWithTag(tag).map(page =>
            el("ul")( // TODO move out
              el("p")(a("post-link", page.targetPath.toString)(page.title))
            )
         )*)
         .build
      )*)
      .build


