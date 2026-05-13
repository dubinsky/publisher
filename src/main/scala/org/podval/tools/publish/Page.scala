package org.podval.tools.publish

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

  final lazy val parent: Option[Directory] = Directory.parent(site, path)
  
  final protected def targetFile: File = path.file(site.targetDirectory)

  def sourcePathOpt: Option[Path]

  def title: String

  def write(): Unit

  def resolveBlock(id: String): Option[Link.ToBlock]

  def resolveSection(names: Seq[String]): Option[Link.ToSection]
  
  def resolveId(id: String): Option[Link.ToId]

object Page:
  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = XmlWriter.xmlWriter.render(xmlContent)
    def xmlContent: Xml.Element

  trait WithHtmlContent extends WithContent:
    final override def content: String = XmlWriter.htmlWriter.render(htmlContent(this))
    def htmlContent(page: Page): Html.Element

  sealed abstract class Fragment(val id: String)

  final class Block(id: String) extends Fragment(id)

  final class Section(
    id: String,
    val title: String,
    val level: Int,
    val sections: Seq[Section]
  ) extends Fragment(id):
    def withSections(sections: Seq[Section]): Section = Section(id, title, level, sections)
