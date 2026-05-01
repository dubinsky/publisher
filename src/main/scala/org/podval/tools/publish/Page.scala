package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.io.File

abstract class Page(
  val site: Site,
  val path: Path
) derives CanEqual:
  override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

  // TODO exact: Boolean
  final def is(url: String): Boolean = paths.exists(_.toString.endsWith(url))

  protected def paths: List[Path]

  final def ref(cls: String): Xml.Element = Link.ToPage(this).a(cls)
  
  def title: String

  final def targetFile: File = path.file(site.targetDirectory)

  def write(): Unit

object Page:
  trait WithSource extends Page:
    def sourcePath: Path

    final override def toString: String =
      val source: String = if sourcePath.withoutExtension == path.withoutExtension then "" else s" ($sourcePath)"
      s"${getClass.getSimpleName} $path$source"

    final override protected def paths: List[Path] = List(
      path,
      path.withoutExtension,
      sourcePath,
      sourcePath.withoutExtension
    )

  trait WithoutSource extends Page:
    final override def toString: String = s"${getClass.getSimpleName} $path"

    final override protected def paths: List[Path] = List(
      path,
      path.withoutExtension
    )

  trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)

    def content: String

  trait WithXmlContent extends WithContent:
    final override def content: String = XmlUtil.write(xmlContent)

    def xmlContent: Xml.Element
