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

  final def ref(cls: String): Xml.Element = Link.ToPage(this).a(cls)

  final def targetFile: File = path.file(site.targetDirectory)

  def isIndex: Boolean = path.fileName == "index" && path.path.length > 1 // TODO "home" is not an index

  // TODO move parent etc. into MarkupPage
  final protected def titleFromPath: String =
    if !isIndex
    then path.fileName
    else parent.map(_.title).getOrElse(path.path.init.last)

  final lazy val parent: Option[Page] =
    // Note: top-level `index` is no-one's parent.
    val parentDirectory: Option[Seq[String]] =
      if path.fileName == "index" && path.path.length > 2 then Some(path.path.init.init)
      else if path.fileName != "index" && path.path.length > 1 then Some(path.path.init)
      else None
    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended("index")*).withExtension(Html.extension)
      site.pages.find(_.path == parentPath) match
        case Some(parent) => parent
        case None => site.addIndexPage(parentPath)

  final lazy val directories: List[Page] = if !isIndex then List.empty else site
    .pages
    .filter(_.path.fileName == "index")
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init)
    .sortBy(_.title)

  final lazy val pages: List[Page] = if !isIndex then List.empty else site
    .pages
    .filter(_.path.fileName != "index")
//    .filter(_.path.path.length > 0)
    .filter(_.path.path.init == path.path.init)
    .sortBy(_.title)

  def title: String

  protected def paths: List[Path]

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
