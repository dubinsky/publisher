package org.podval.tools.publish

import java.io.File

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:
  def sourcePathOpt: Option[Path]
  
  def title: String

  def write(): Unit

  final override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

  final override def hashCode(): Int = path.hashCode()
  
  final override def toString: String =
    val source: String = sourcePathOpt match
      case Some(sourcePath) if sourcePath.path != path.path  => s" ($sourcePath)"
      case _ => ""

    s"${getClass.getSimpleName} $path$source"

  final def is(path: Path, isAbsolute: Boolean): Boolean =
    this.path.is(path, isAbsolute) || sourcePathOpt.exists(_.is(path, isAbsolute))

  final lazy val parent: Option[Directory] = Directory.parent(site, path)

  final def link: Page.Link = Page.Link(this, part = None)

  final def ref(cls: String): Html.Element =
    val pageLink = link
    import zio.blocks.html.*
    a(className := cls, href := pageLink.url, pageLink.title)

  final def targetFile: File = path.file(site.targetDirectory)

  // TODO move into site together with Ref
  final def resolveRef(fragment: Option[String]): Option[Page.Link] = fragment match
    case None => Some(link)
    case Some(fragment) =>  Some(Page.Link(this,
      if fragment.startsWith("^")
      then resolveBlock(id = fragment.substring(1).trim)
      else resolveSection(names = fragment.split('#').map(_.trim).toSeq)
    ))

  def resolveBlock(id: String): Option[Page.PartLink.ToBlock]
  def resolveSection(names: Seq[String]): Option[Page.PartLink.ToSection]

object Page:
  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = XmlWriter.xmlWriter.render(xmlContent)
    def xmlContent: Xml.Element

  trait WithHtmlContent extends WithContent:
    final override def content: String = XmlWriter.htmlWriter.render(htmlContent)
    def htmlContent: Html.Element
  
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

  final class Link(val page: Page, part: Option[PartLink]):
    def url: String = page.path.toString + part.fold("")(part => s"#${part.id}")
    def title: String = page.title + part.fold("")(part => s"#${part.title}")

  sealed abstract class Part(val id: String)

  final class Block(id: String) extends Part(id)

  object Block:
    val className: String = "wiki-block"

  final class Section(
    id: String,
    val title: String,
    val level: Int,
    val sections: Seq[Section]
  ) extends Part(id):
    def withSections(sections: Seq[Section]): Section = Section(id, title, level, sections)

  sealed abstract class PartLink:
    def title: String
    def id: String

  object PartLink:
    final class ToBlock(block: Block) extends PartLink:
      override def id: String = block.id
      override def title: String = s"^${block.id}"

    final class ToSection(sections: Seq[Section]) extends PartLink:
      override def id: String = sections.last.id
      override def title: String = sections.map(_.title).mkString("#")
