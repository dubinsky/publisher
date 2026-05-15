package org.podval.tools.publish.pages

import org.podval.tools.publish.util.Icon
import org.podval.tools.publish.{FrontMatter, MarkupPage, MarkupSource, Page, Path, Site}
import org.podval.xml.{Html, Xml}
import zio.blocks.html.*

object Tags:
  object Maker extends MarkupPage.AutoMaker[Tags](Path("tags").html, Tags.apply)

final class Tags(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  source: Option[MarkupSource]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  source
):
  override protected def titleDefault: String = "Tags"
  override protected def descriptionDefault: Option[String] = Some("Pages by tags")
  override protected def iconDefault: Icon = Icon.tags
  override protected def headerPagePriorityDefault: Int = 2
  override protected def langDefault: Option[String] = Some("en")

  private def tagsAll: List[String] = site.markupPages.flatMap(_.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Html.Element = a(
    className := "page-tag",
    href := s"$path#${Xml.Id.toId(tag)}",
    Icon.tag.htmlSpan,
    tag
  )

  override def isSynthetic: Boolean = true
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
