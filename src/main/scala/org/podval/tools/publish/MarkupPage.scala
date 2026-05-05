package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import scala.reflect.TypeTest
import java.time.LocalDate
import XmlUtil.{a, apply, child, div, el, withText}

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

  // TODO get dates from GIT too
  private lazy val postDate: Option[LocalDate] = Post.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = None // TODO

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

  final override def xmlContent: Xml.Element = Minima.render(
    page = this,
    content = article(Seq(pageMarkup.map(_.xmlContent), syntheticContent).flatten)
  )

  // TODO I may have to rip out the titles at layout...
  private def article(content: Seq[Xml.Element]): Xml.Element =
    el("article", "class" -> "post h-entry", "itemscope" -> "", "itemtype" -> "http://schema.org/BlogPosting")(
      el("header", "class" -> "post-header")
        .child(el("h1", "class" -> "post-title p-name", "itemprop" -> "name headline").withText(title))
        .child(Option.when(!this.isInstanceOf[MarkupPage.BaseLayout])(articleMeta))
        .build,
      div("post-content e-content", "itemprop" -> "articleBody")(content*),
      // Note: skipped Disqus comments for posts
      a("u-url", path.toString).attr("hidden", "")()
    )

  // Note: in Minima, pages do not get any metadata; I added tags - but why not have all of it?
  private def articleMeta: Xml.Element =
    div("post-meta")
      .child(Option.when(dateModified.isDefined)(
        el("span", "class" -> "meta-label").withText("Published:")
      ))
      .child(
        time(date, "dt-published", "datePublished")
      )
      .child(Option.when(dateModified.isDefined)(
        el("span")(
          el("span", "class" -> "bullet-divider").withText("•"),
          el("span", "class" -> "meta-label").withText("Updated:")
        )
      ))
      .child(
        time(dateModified, "dt-modified", "dateModified")
      )
      .child(
        // TODO do a div like with author - or rather, use span for both and remove display: inline from CSS
        Option.when(tags.nonEmpty):
          el("span")
            .child(el("span", "class" -> "pipe-divider").withText("|"))
            .children(tags.map(site.tags.tagRef)*)
            .build
      )
      .child(
        // TODO change into a span and get rid of all this force-inline business
        // TODO s"${if page.frontMatter.modified_date then "" else "force-inline "}post-authors"
        div("post-authors")(
          // Note: only one author is supported: me :)
          el("span", "class" -> "bullet-divider").withText("•"),
          el("span", "itemprop" -> "author", "itemscope" -> "", "itemtype" -> "http://schema.org/Person")(
            el("span", "class" -> "p-author h-card", "itemprop" -> "name").withText(author)
          )
        )
      )
      .build

  private def time(
    date: Option[Date],
    cls: String,
    itemprop: String
  ): Option[Xml.Element] = date.map: date =>
    el("time",
      "class" -> cls,
      "datetime" -> date.toString,
      "itemprop" -> itemprop
    )
      .withText(date.toShortString)

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


