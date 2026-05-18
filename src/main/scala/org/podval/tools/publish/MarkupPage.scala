package org.podval.tools.publish

import org.podval.tools.publish.util.{Date, Icon}
import org.podval.xml.Html
import java.time.LocalDate

// TODO move all methods to Page?
open class MarkupPage(
  site: Site,
  path: Path
) extends Page(
  site,
  path
) with Page.WithContent:
  override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note

  private lazy val postDate: Option[LocalDate] = Posts.date(path)
  final def isPost: Boolean = postDate.isDefined
  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)
  final def dateModified: Option[Date] = frontMatter.modified_time

  // TODO override in Directory
  override protected def titleDefault: String =
    if !isDirectory then super.titleDefault else postDate match
      case Some(postDate) => postDate.toString // daily note
      case None => fileName

  private var source: Option[MarkupSource] = None
  def withSource(source: MarkupSource): Unit = this.source = Some(source)
  
  final override def sourcePathOpt: Option[Path] =
    source.map(_.sourcePath)

  final override def frontMatter: FrontMatter =
    source.map(_.cached.frontMatter).getOrElse(FrontMatter.absent)
    
  final def backLinks: Seq[BackLinks.BackLink] =
    source.map(_.cached.backLinks(this)).getOrElse(Seq.empty)

  final override def resolveBlock(id: String): Option[Link.ToBlock] =
    source.flatMap(_.cached.resolveBlock(id))

  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] =
    source.flatMap(_.cached.resolveSection(names))

  final override def resolveId(id: String): Option[Link.ToId] =
    source.flatMap(_.cached.resolveId(id))

  final override def content: String =
    val html: Html.Element = Minima.render(
      page = this,
      markupContent = source.map(_.cached.htmlContent(this)),
      syntheticContent = syntheticContent
    )
    Html.writer.render(html)

  def isSynthetic: Boolean = false
  protected def syntheticContent: Option[Html.Element] = None
