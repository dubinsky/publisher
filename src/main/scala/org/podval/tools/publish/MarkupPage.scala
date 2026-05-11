package org.podval.tools.publish

import scala.reflect.TypeTest
import java.time.LocalDate

open class MarkupPage(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends Page(
  site,
  path
) with Page.WithHtmlContent:
  final def tags: List[String] = frontMatter.tags
  final def author: String = frontMatter.author.getOrElse(site.author)
  final def lang: String = frontMatter.lang.getOrElse(site.lang)
  final def math: Boolean = frontMatter.math
  final def icon: FontAwesome.Icon = frontMatter.icon.getOrElse(iconDefault)
  def iconDefault: FontAwesome.Icon = FontAwesome.note

  final lazy val headerPage: Option[Site.HeaderPage] = frontMatter
    .headerPage
    .filter(_.include)
    .map(headerPage => Site.HeaderPage(
      page = this,
      priority = headerPage.priority.getOrElse(0)
    ))

  private lazy val postDate: Option[LocalDate] = Post.date(path)
  final def isPost: Boolean = postDate.isDefined
  // TODO get dates from GIT too
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modified_time

  final override def title: String = frontMatter.title.getOrElse:
    if !Directory.is(path) then path.fileNameWithNonHtmlExtension else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None if path.path.length > 1 => path.path.init.last // directory name
      case None => path.fileName // "index"

  final override def sourcePathOpt: Option[Path] =
    pageMarkup.map(_.sourcePath)

  final def backLinks: Seq[BackLinks.BackLink] =
    pageMarkup.fold(Seq.empty)(_.backLinks(this))

  final override def resolveBlock(id: String): Option[Page.PartLink.ToBlock] =
    pageMarkup.flatMap(_.resolveBlock(id))

  final override def resolveSection(names: Seq[String]): Option[Page.PartLink.ToSection] =
    pageMarkup.flatMap(_.resolveSection(names))

  final override def htmlContent: Html.Element =
    val markupContent: Option[Html.Element] = pageMarkup.map: pageMarkup =>
      val htmlContent: Html.Element = Html.fromXml(pageMarkup.xmlContent)
      Html.transform(htmlContent, pageMarkup.markup.stop, element =>
        if !MarkupPage.isKramdownTocMarker(element)
        then element
        else MarkupPage.toc(pageMarkup.sections)
      )

    Minima.render(
      page = this,
      content = Seq(markupContent, syntheticContent).flatten
    )

  protected def syntheticContent: Option[Html.Element] = None

object MarkupPage:
  trait BaseLayout extends MarkupPage

  abstract class Maker[P <: MarkupPage](
    make: (site: Site, path: Path, frontMatter: FrontMatter, pageMarkup: Option[PageMarkup]) => P
  ):
    def path(sourcePath: Path): Option[Path]

    def frontMatterDefault: FrontMatter = FrontMatter.empty

    final def withSource(
      site: Site,
      frontMatter: FrontMatter,
      pageMarkup: PageMarkup
    ): Option[P] = path(pageMarkup.sourcePath).map(path => make(
      site,
      path.html,
      frontMatter.merge(frontMatterDefault),
      Some(pageMarkup)
    ))

  object DefaultMaker extends Maker[MarkupPage](MarkupPage.apply):
    override def path(sourcePath: Path): Some[Path] = Some(sourcePath)

  abstract class AutoMaker[P <: MarkupPage](
    path: Path,
    make: (site: Site, path: Path, frontMatter: FrontMatter, pageMarkup: Option[PageMarkup]) => P,
    override val frontMatterDefault: FrontMatter
  )(using TypeTest[MarkupPage, P]) extends Maker[P](make):
    final override def path(sourcePath: Path): Option[Path] = Option.when(sourcePath.html == path)(sourcePath)

    final def get(site: Site): P = site.getOrElse(path):
      make(
        site,
        path,
        frontMatter = frontMatterDefault,
        pageMarkup = None
      )

  private def isKramdownTocMarker(element: Html.Element): Boolean =
    Html.qName(element) == "ul" &&
    Html.children(element).length == 1 &&
    Html.asElement(Html.children(element).head).fold(false): child =>
      Html.qName(child) == "li" &&
      Html.children(child).length == 1 &&
      Html.asText(Html.children(child).head).fold(false): text =>
        text.endsWith("{:toc}")

  private def toc(sections: Seq[Page.Section]): Html.Element =
    import zio.blocks.html.*
    div(className := "toc",
      h3("Table of Contents"),
      tocSections(sections)
    )

  private def tocSections(sections: Seq[Page.Section]): Html.Element =
    import zio.blocks.html.*
    ul(sections.map(section => 
      li(
        a(className := "toc-link", href := s"#${section.id}", section.title),
        Option.when(section.sections.nonEmpty)(tocSections(section.sections))
      )  
    ))
