package org.podval.tools.publish

import org.podval.tools.publish.util.{Files, Icon}
import org.podval.xml.{Html, Xml, XmlWriter}
import zio.blocks.html.*
import java.io.File

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:

  final override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

  final override def hashCode(): Int = path.hashCode()

  final override def toString: String =
    val source: String = sourcePathOpt match
      case Some(sourcePath) if sourcePath.path != path.path  => s" ($sourcePath)"
      case _ => ""

    s"${getClass.getSimpleName} $path$source"

  final def isDirectory: Boolean = Directory.is(path)

  final def fileName: String = if !isDirectory then path.fileName else
    if path.path.length > 1
    then path.path.init.last
    else path.fileName // "index"

  final lazy val parent: Option[Directory] = Directory.parent(site, this)
  
  final protected def targetFile: File = path.file(site.targetDirectory)

  final def ref(
    cls: Option[String] = None,
    withTitle: Boolean = true,
    withIcon: Boolean = true,
    icon: Option[Icon] = None
  ): Html.Element =
    val pageLink: Link = Link(this, fragment = None, intrapage = false)
    a(
      className := "page-ref",
      cls.map(cls => className += cls),
      href := pageLink.url,
      Option.when(withIcon)(icon.getOrElse(this.icon).htmlSpan),
      Option.when(withTitle)(pageLink.title)
    )

  def sourcePathOpt: Option[Path]

  def title: String

  def aliases: List[String]

  def icon: Icon

  def write(): Unit

  def resolveBlock(id: String): Option[Link.ToBlock]

  def resolveSection(names: Seq[String]): Option[Link.ToSection]
  
  def resolveId(id: String): Option[Link.ToId]

object Page:
  def pageList(pages: Seq[Page], cls: Option[String] = None): Html.Element = ul(
    className := "page-list",
    pages.map(page => li(page.ref(cls = cls)))
  )

  sealed abstract class Fragment(val id: String)

  final class Block(id: String) extends Fragment(id)

  final class Section(
    id: String,
    val title: String,
    val sections: Seq[Section]
  ) extends Fragment(id)

  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = Xml.writer.render(xmlContent)
    def xmlContent: Xml.Element

  trait WithHtmlContent extends WithContent:
    final override def content: String = Html.writer.render(htmlContent)
    def htmlContent: Html.Element

  sealed abstract class Asset(site: Site, path: Path) extends Page(site: Site, path: Path):
    final override def sourcePathOpt: Option[Path] = None
    final override def title: String = path.fileNameWithNonHtmlExtension
    final override def aliases: List[String] = List.empty
    final override def icon: Icon = Icon.file
    final override def resolveBlock(id: String): Option[Link.ToBlock] = None
    final override def resolveSection(names: Seq[String]): Option[Link.ToSection] = None
    final override def resolveId(id: String): Option[Link.ToId] = None

  final class AssetWithSource(site: Site, path: Path) extends Asset(site, path):
    override def write(): Unit = Files.copy(fromFile = path.file(site.sourceDirectory), toFile = targetFile)

  abstract class SyntheticAsset(site: Site, path: Path) extends Asset(site, path)
    with WithContent

  abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path)
    with WithXmlContent

  final class EmbeddedAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
    override def content: String = Files.readResource(Site.resourcesBase + path.toString)
