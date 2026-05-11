package org.podval.tools.publish

import scala.ref.SoftReference
import scala.reflect.TypeTest
import java.time.LocalDate

open class MarkupPage(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  source: Option[MarkupPage.Source]
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
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modified_time

  final override def title: String = frontMatter.title.getOrElse:
    if !Directory.is(path) then path.fileNameWithNonHtmlExtension else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None if path.path.length > 1 => path.path.init.last // directory name
      case None => path.fileName // "index"

  final override def sourcePathOpt: Option[Path] =
    source.map(_.sourcePath)

  final def backLinks: Seq[BackLinks.BackLink] =
    source.fold(Seq.empty)(_.cached.backLinks(this))

  final override def resolveBlock(id: String): Option[Link.ToBlock] =
    source.flatMap(_.cached.resolveBlock(id))

  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    source.flatMap(_.cached.resolveSection(names))

  final override def htmlContent: Html.Element = Minima.render(
    page = this,
    markupContent = source.map(_.cached.htmlContent),
    syntheticContent = syntheticContent
  )

  protected def syntheticContent: Option[Html.Element] = None

object MarkupPage:
  trait BaseLayout extends MarkupPage

  final class Source(
    val markup: Markup,
    val site: Site,
    val sourcePath: Path
  ):
    private var cachedVar: Option[SoftReference[PageMarkup]] = None
    private def cache(cached: PageMarkup): Unit = cachedVar = Some(SoftReference(cached))

    def cache(xml: Xml.Element): Unit = cache(PageMarkup(this, xml))

    def cached: PageMarkup = cachedVar.get.get.getOrElse:
      site.log.warn(s"Re-reading evicted PageMarkup: $sourcePath")
      val (frontMatter: FrontMatter, xml: Xml.Element) = site.parseMarkup(sourcePath, markup)
      val cached: PageMarkup = PageMarkup(this, xml)
      cache(cached)
      cached

  abstract class Maker[P <: MarkupPage](
    make: (site: Site, path: Path, frontMatter: FrontMatter, source: Option[Source]) => P
  ):
    def path(sourcePath: Path): Option[Path]

    def frontMatterDefault: FrontMatter = FrontMatter.empty

    final def withSource(
      site: Site,
      frontMatter: FrontMatter,
      source: Source
    ): Option[P] = path(source.sourcePath).map(path => make(
      site,
      path.html,
      frontMatter.merge(frontMatterDefault),
      Some(source)
    ))

  object DefaultMaker extends Maker[MarkupPage](MarkupPage.apply):
    override def path(sourcePath: Path): Some[Path] = Some(sourcePath)

  abstract class AutoMaker[P <: MarkupPage](
    path: Path,
    make: (site: Site, path: Path, frontMatter: FrontMatter, source: Option[Source]) => P,
    override val frontMatterDefault: FrontMatter
  )(using TypeTest[MarkupPage, P]) extends Maker[P](make):
    final override def path(sourcePath: Path): Option[Path] = Option.when(sourcePath.html == path)(sourcePath)

    final def get(site: Site): P = site.getOrElse(path):
      make(
        site,
        path,
        frontMatter = frontMatterDefault,
        source = None
      )
