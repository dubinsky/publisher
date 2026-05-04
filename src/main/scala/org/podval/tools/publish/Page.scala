package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.io.File
import XmlUtil.{childrenWhenEmpty, replaceAttribute, withText}

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:
  def sourcePathOpt: Option[Path]

  def resolveFragment(fragment: String): Option[Toc.Link]

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

  final def ref(cls: String): Xml.Element = Page.Link(this, part = None).a(cls)

  final def targetFile: File = path.file(site.targetDirectory)

  final def resolveRef(fragment: Option[String]): Option[Page.Link] = fragment match
    case None => Some(Page.Link(this, part = None))
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

  final class Link(val page: Page, part: Option[Toc.Link]):
    def url: String = page.path.toString + part.fold("")(part => s"#${part.id}")

    def title: String = page.title + part.fold("")(part => s"#${part.title}")

    def a(cls: String): Xml.Element = XmlUtil.a(cls, url).withText(title)

    def a(element: Xml.Element): Xml.Element = element
      .copy(name = XmlUtil.a)
      .replaceAttribute(XmlUtil.hrefAttribute, XmlUtil.escapeUrl(url))
      .childrenWhenEmpty(Some(title))

  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = XmlUtil.write(xmlContent)
    def xmlContent: Xml.Element
