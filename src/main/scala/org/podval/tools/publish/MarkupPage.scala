package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import scala.reflect.TypeTest
import java.time.LocalDate

open class MarkupPage(
  site: Site,
  path: Path,
  val frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends Page(
  site,
  path
) with Page.WithXmlContent:

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
    if Directory.is(path) && path.path.length > 1
    // TODO look up *that* title  
    then path.path.init.last
    // TODO add file extension if it is not .html
    else path.fileName

  final def resolveLinks(): Unit = pageMarkup.foreach(_.resolveLinks(this))

  private lazy val postDate: Option[LocalDate] = Post.date(path)

  final def isPost: Boolean = postDate.isDefined

  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)

  final def author: Option[String] = frontMatter.author // TODO default to site.author?

  final lazy val parent: Option[Directory] = Directory.parent(site, path)

object MarkupPage:
  trait BaseLayout extends MarkupPage

  abstract class Maker(site: Site):
    def withSource(
      // TODO package together?
      sourcePath: Path,
      markup: Markup,
      frontMatter: FrontMatter,
      xml: Xml.Element
    ): Option[MarkupPage]

  final class DefaultMaker(site: Site) extends Maker(site):
    override def withSource(
      sourcePath: Path,
      markup: Markup,
      frontMatter: FrontMatter,
      xml: Xml.Element
    ): Option[MarkupPage] = Some(MarkupPage(
      site = site,
      path = sourcePath.html,
      frontMatter = frontMatter,
      pageMarkup = Some(PageMarkup(
        sourcePath,
        markup,
        xml
      ))
    ))

  abstract class AutoMaker[P <: MarkupPage](site: Site, path: Path)(using TypeTest[MarkupPage, P]) extends Maker(site):
    final def get: P = site.getOrElse(path)(withoutSource)

    final override def withSource(
      sourcePath: Path,
      markup: Markup,
      frontMatter: FrontMatter,
      xml: Xml.Element
    ): Option[MarkupPage] = Option.when(sourcePath.html == path):
      withSource(
        frontMatter = frontMatter,
        pageMarkup = PageMarkup(sourcePath, markup, xml)
      )

    def withSource(
      pageMarkup: PageMarkup,
      frontMatter: FrontMatter
    ): P

    def withoutSource: P
