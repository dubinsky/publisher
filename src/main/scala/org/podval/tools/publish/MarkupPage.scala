package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}
import scala.reflect.TypeTest
import java.time.LocalDate
import XmlUtil.{a, apply, attr, child, div, el, withText}

open class MarkupPage(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends Page(
  site,
  path
) with Page.WithXmlContent:

  final def tags: List[String] = frontMatter.tags
  final def author: String = frontMatter.author.getOrElse(site.author)
  final def lang: String = frontMatter.lang.getOrElse(site.lang)
  final def math: Boolean = frontMatter.math

  private lazy val postDate: Option[LocalDate] = Post.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)

  final override def title: String = frontMatter.title.getOrElse:
    if !Directory.is(path) then path.fileNameWithNonHtmlExtension else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None if path.path.length > 1 => path.path.init.last // directory name
      case None => path.fileName // "index"

  final def resolveLinks(): Unit =
    pageMarkup.foreach(_.resolveLinks(this))

  final override def sourcePathOpt: Option[Path] =
    pageMarkup.map(_.sourcePath)

  final override def resolveFragment(fragment: String): Option[Toc.Link] =
    pageMarkup.flatMap(_.resolveFragment(fragment))

  protected def syntheticContent: Option[Xml.Element] = None

  final override def xmlContent: Xml.Element =
    // TODO bring page and post metadata closer to one another
    // Note: I need a 'div' to combine page content with synthesized one:
    val articleContent: Xml.Element =
      div(if isPost then "post-content e-content" else "post-content")
        .attr(Option.when(isPost)("itemprop" -> "articleBody"))
        .children(Seq(pageMarkup.map(_.xmlContent), syntheticContent).flatten*)
        .build

    Minima.render(this, content =
      if this.isInstanceOf[MarkupPage.BaseLayout]
      then articleContent
      else article(articleContent)
    )

  private def article(content: Xml): Xml.Element =
    el("article", "class" -> (if !isPost then "post" else "post h-entry"))
      .attr(Option.when(isPost)("itemscope", ""))
      .attr(Option.when(isPost)("itemtype", "http://schema.org/BlogPosting"))
      .child(
        el("header", "class" -> "post-header")
          .child(
            el("h1", "class" -> (if !isPost then "post-title" else "post-title p-name"))
              .attr(Option.when(isPost)("itemprop", "name headline"))
              .withText(title)
          )
          .child(articleMeta)
          .build
      )
      .child(content)
      // Note: skipped Disqus comments for posts
      .child(Option.when(isPost)(a("u-url", path.toString).attr("hidden", "")()))
      .build

  private def articleMeta: Option[Xml.Element] =
    if !isPost then
      tagsSpan
    else Some:
      div("post-meta")
        // TODO
        // .childWhen(page.modified_date.isDefined,
        //    el("span", "class" -> "meta-label")("Published:")
        // )
        .child(date.map: date =>
          el("time",
            "class" -> "dt-published",
            "datetime" -> date.toString,
            "itemprop" -> "datePublished"
          )
            .withText(date.toShortString)
        )
        // TODO
        // {%- if page.modified_date -%}
        //   <span class="bullet-divider">•</span>
        //   <span class="meta-label">Updated:</span>
        //   {%- assign mdate = page.modified_date | date_to_xmlschema %}
        //   <time class="dt-modified" datetime="{{ mdate }}" itemprop="dateModified">
        //     {{ mdate | date: date_format }}
        //   </time>
        // {%- endif -%}
        .child(tagsSpan)
        .child(
          div("post-authors")( // TODO s"${if page.frontMatter.modified_date then "" else "force-inline "}post-authors"
        //    TODO multiple authors, separated by commas?
            XmlBuilder.text("•"),
            el("span", "itemprop" -> "author", "itemscope" -> "", "itemtype" -> "http://schema.org/Person")(
              el("span", "class" -> "p-author h-card", "itemprop" -> "name").withText(author)
            )
          )
        )
        .build

  // TODO do a div like with author
  private def tagsSpan: Option[Xml.Element] = Option.when(tags.nonEmpty):
    el("span")
      .child(XmlBuilder.text("|"))
      .children(tags.map(site.tags.tagRef)*)
      .build

object MarkupPage:
  trait BaseLayout extends MarkupPage

  abstract class Maker[P <: MarkupPage](
    make: (site: Site, path: Path, frontMatter: FrontMatter, pageMarkup: Option[PageMarkup]) => P
  ):
    def path(sourcePath: Path): Option[Path]

    final def withSource(
      site: Site,
      frontMatter: FrontMatter,
      pageMarkup: PageMarkup
    ): Option[P] = path(pageMarkup.sourcePath).map(path => make(
      site,
      path.html,
      frontMatter,
      pageMarkup = Some(pageMarkup)
    ))

  object DefaultMaker extends Maker[MarkupPage](MarkupPage.apply):
    override def path(sourcePath: Path): Some[Path] = Some(sourcePath)

  abstract class AutoMaker[P <: MarkupPage](
    path: Path,
    make: (site: Site, path: Path, frontMatter: FrontMatter, pageMarkup: Option[PageMarkup]) => P,
    frontMatterWithoutSource: FrontMatter
  )(using TypeTest[MarkupPage, P]) extends Maker[P](make):
    final override def path(sourcePath: Path): Option[Path] = Option.when(sourcePath.html == path)(sourcePath)

    final def get(site: Site): P = site.getOrElse(path):
      make(
        site,
        path,
        frontMatter = frontMatterWithoutSource,
        pageMarkup = None
      )


