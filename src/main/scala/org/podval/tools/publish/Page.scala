package org.podval.tools.publish

import java.io.File

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:
  def sourcePathOpt: Option[Path]

  def resolveFragment(fragment: String): Option[PageMarkup.Link]

  def title: String

  def write(): Unit

  final override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

  final override def toString: String =
    val source: String = sourcePathOpt match
      case Some(sourcePath) if sourcePath.path != path.path  => s" ($sourcePath)"
      case _ => ""

    s"${getClass.getSimpleName} $path$source"

  final def is(path: Path, isAbsolute: Boolean): Boolean =
    this.path.is(path, isAbsolute) || sourcePathOpt.exists(_.is(path, isAbsolute))

  final lazy val parent: Option[Directory] = Directory.parent(site, path)

  final def link: Page.Link = Page.Link(this, part = None)

  // TODO unfold
  final def ref(cls: String): Html.Element =
    val pageLink = link
    import zio.blocks.html.*
    a(className := cls, href := pageLink.url, pageLink.title)

  final def targetFile: File = path.file(site.targetDirectory)

  // TODO simplify
  final def resolveRef(fragment: Option[String]): Option[Page.Link] = fragment match
    case None => Some(link)
    case Some(fragment) => resolveFragment(fragment) match
      case Some(part) => Some(Page.Link(this, Some(part)))
      case None => None

object Page:
  final class Ref(
    val path: Path, // could be `name`, `path/name`(?)
    val isAbsolute: Boolean,
    val fragment: Option[String] // could be `#section`, `#section#subsection`, `#^block`
  )

  object Ref:
    def apply(ref: String): Ref =
      val (pathString: String, fragment: Option[String]) = Strings.split(ref, '#')
      val pathSegments: Seq[String] = pathString.trim.split('/').toSeq.filterNot(_.isEmpty).map(_.trim)
      val path: Path = if pathSegments.isEmpty then Path.root else
        val (lastSegment, extension) = Files.nameAndExtension(pathSegments.last)
        Path(pathSegments.init :+ lastSegment.trim, extension)
      val isAbsolute: Boolean = pathString.trim.startsWith("/")
      new Ref(path, isAbsolute, fragment)

  final class Link(val page: Page, part: Option[PageMarkup.Link]):
    def url: String = page.path.toString + part.fold("")(part => s"#${part.id}")
    def title: String = page.title + part.fold("")(part => s"#${part.title}")

  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = XmlWriter.xmlWriter.render(xmlContent)
    def xmlContent: Xml.Element

  trait WithHtmlContent extends WithContent:
    final override def content: String = XmlWriter.htmlWriter.render(htmlContent)
    def htmlContent: Html.Element

