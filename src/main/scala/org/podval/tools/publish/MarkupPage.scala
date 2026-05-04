package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
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
) with Page.WithXmlContent:

  final def tags: List[String] = frontMatter.tags
  final def author: String = frontMatter.author.getOrElse(site.author)
  final def lang: String = frontMatter.lang.getOrElse(site.lang)
  final def math: Boolean = frontMatter.math

  private lazy val postDate: Option[LocalDate] = Post.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)

  final def resolveLinks(): Unit =
    pageMarkup.foreach(_.resolveLinks(this))

  final override def sourcePathOpt: Option[Path] =
    pageMarkup.map(_.sourcePath)

  final override def resolveFragment(fragment: String): Option[Toc.Link] =
    pageMarkup.flatMap(_.resolveFragment(fragment))

  final override def xmlContent: Xml.Element =
    Minima(this, syntheticContent(pageMarkup.map(_.xmlContent))).render

  // TODO leave undefined to force subclasses to incorporate content
  protected def syntheticContent(content: Option[Xml.Element]): Xml.Element =
    content.getOrElse(XmlUtil.invisible)

  final override def title: String = frontMatter.title.getOrElse:
    if !Directory.is(path) then path.fileNameWithNonHtmlExtension else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None if path.path.length > 1 => path.path.init.last // directory name
      case None => path.fileName // "index"

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


