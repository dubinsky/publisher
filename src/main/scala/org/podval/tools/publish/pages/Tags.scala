package org.podval.tools.publish.pages

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{FrontMatter, MarkupPage, Page, Path, Site}
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*

final class Tags(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  source: Option[MarkupPage.Source]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  source
):
  override def isSynthetic: Boolean = true

  override def iconDefault: Icon = Icon.tags

  private def tagsAll: List[String] = site.markupPages.flatMap(_.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Html.Element = a(
    className := "page-tag",
    href := s"$path#${Xml.Id.toId(tag)}",
    Icon.tag.htmlSpan,
    tag
  )

  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "tags",
      h2("All tags"),
      p(tagsAll.map(tagRef)),
      h2("Pages by tags"),
      ul(tagsAll.map(tag =>
        li(
          h3(className := "page-tag", id := Xml.Id.toId(tag), tag),
          Page.pageList(withTag(tag), cls = Some("post-link"))
        )
      ))
    )

object Tags:
  object Maker extends MarkupPage.AutoMaker[Tags](
    path = Path("tags").html,
    make = Tags.apply,
    frontMatterDefault = FrontMatter(
      title = Some("Tags"),
      description = Some("Pages by tags"),
      lang = Some("en"),
      //    permalink = Some(path.withoutExtension.toString)
      headerPage = Some(FrontMatter.HeaderPage(priority = Some(2)))
    )
  )
