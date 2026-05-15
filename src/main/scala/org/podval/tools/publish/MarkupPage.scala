package org.podval.tools.publish

import org.podval.tools.publish.util.{Date, Icon}
import org.podval.xml.Html
import scala.reflect.TypeTest
import java.time.LocalDate

open class MarkupPage(
  site: Site,
  path: Path,
  override val frontMatter: FrontMatter,
  source: Option[MarkupSource]
) extends Page(
  site,
  path
) with Page.WithHtmlContent:
  override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note

  private lazy val postDate: Option[LocalDate] = Post.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modified_time

  override protected def titleDefault: String =
    if !isDirectory then super.titleDefault else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None => fileName

  final override def sourcePathOpt: Option[Path] =
    source.map(_.sourcePath)

  final def backLinks: Seq[BackLinks.BackLink] =
    source.fold(Seq.empty)(_.cached.backLinks(this))

  final override def resolveBlock(id: String): Option[Link.ToBlock] =
    source.flatMap(_.cached.resolveBlock(id))

  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    source.flatMap(_.cached.resolveSection(names))

  final override def resolveId(id: String): Option[Link.ToId] =
    source.flatMap(_.cached.resolveId(id))

  final override def htmlContent: Html.Element = Minima.render(
    page = this,
    markupContent = source.map(_.cached.htmlContent(this)),
    syntheticContent = syntheticContent
  )

  def isSynthetic: Boolean = false
  protected def syntheticContent: Option[Html.Element] = None

object MarkupPage:
  abstract class Maker[P <: MarkupPage](
    make: (site: Site, path: Path, frontMatter: FrontMatter, source: Option[MarkupSource]) => P
  ):
    def path(sourcePath: Path): Option[Path]

    final def withSource(
      site: Site,
      frontMatter: FrontMatter,
      source: MarkupSource
    ): Option[P] = path(source.sourcePath).map(path => make(
      site,
      path.html,
      frontMatter,
      source = Some(source)
    ))

  object DefaultMaker extends Maker[MarkupPage](MarkupPage.apply):
    override def path(sourcePath: Path): Some[Path] = Some(sourcePath)

  abstract class AutoMaker[P <: MarkupPage](
    path: Path,
    make: (site: Site, path: Path, frontMatter: FrontMatter, source: Option[MarkupSource]) => P
  )(using TypeTest[MarkupPage, P]) extends Maker[P](make):
    final override def path(sourcePath: Path): Option[Path] = Option.when(sourcePath.html == path)(sourcePath)

    final def get(site: Site): P = site.getOrElse(path):
      make(
        site,
        path,
        FrontMatter.absent,
        source = None
      )
