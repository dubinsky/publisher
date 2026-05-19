package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html

abstract class MarkupPage(site: Site, path: Path) extends Page(site, path) with Page.WithContent:
  override protected def titleDefault: String = path.fileName

  private var sourceVar: Option[MarkupSource] = None
  final override protected def source: Option[MarkupSource] = sourceVar
  
  def withSource(
    markup: Markup,
    sourcePath: Path
  ): Unit = this.sourceVar = Some(MarkupSource(
    site = site,
    markup = markup,
    sourcePath = sourcePath
  ))

  final override def sourcePath: Option[Path] = source.map(_.sourcePath)

  final def backLinks: Seq[BackLinks.BackLink] = source.map(_.cached.backLinks(this)).getOrElse(Seq.empty)

  final override def content: String =
    val html: Html.Element = Minima.render(
      page = this,
      markupContent = source.map(_.cached.htmlContent(this)),
      syntheticContent = syntheticContentOpt
    )
    Html.writer.render(html)

  def hasSyntheticContent: Boolean

  protected def syntheticContentOpt: Option[Html.Element]

object MarkupPage:
  final class Simple(site: Site, path: Path) extends MarkupPage(site, path):
    override protected def iconDefault: Icon = if isPost then Icon.envelope else Icon.note
    override def isDirectory: Boolean = false
    override def hasSyntheticContent: Boolean = false
    override protected def syntheticContentOpt: Option[Html.Element] = None

  abstract class WithSyntheticContent(site: Site, path: Path) extends MarkupPage(site, path):
    final override def hasSyntheticContent: Boolean = true
    final override protected def syntheticContentOpt: Option[Html.Element] = Some(syntheticContent)
    protected def syntheticContent: Html.Element


