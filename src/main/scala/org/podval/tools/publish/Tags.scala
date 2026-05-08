package org.podval.tools.publish

import zio.blocks.html.*

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
  override def iconDefault: FontAwesome.Icon = FontAwesome.tags

  private def tagsAll: List[String] = site.markupPages.flatMap(_.tags).distinct.sorted

  private def withTag(tag: String): List[Page] = site.markupPages.filter(_.tags.contains(tag)).sortBy(_.title)

  def tagRef(tag: String): Html.Element = a(className := "page-tag", href := s"$path#${Xml.toId(tag)}", tag)

  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "tags",
      h2("All tags"),
      p(tagsAll.map(tagRef)),
      h2("Pages by tags"),
      ul(tagsAll.map(tag =>
        li(
          h3(className := "page-tag", id := Xml.toId(tag), tag),
          ul(className := "tag-pages-list", withTag(tag).map(page => li(page.ref("post-link"))))
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
