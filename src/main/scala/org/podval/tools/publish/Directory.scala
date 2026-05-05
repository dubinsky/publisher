package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{apply, child, div, el, ul, withText}

final class Directory(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup
) with MarkupPage.BaseLayout:
  override protected def syntheticContent: Option[Xml.Element] = Some:
    div("directory")
      .child(pageList(directories, "Directories", "directories-list"))
      .child(pageList(pages, "Pages", "pages-list"))
      .build

  private def pageList(
    pages: List[Page],
    title: String,
    cls: String
  ): Option[Xml.Element] = Option.when(pages.nonEmpty):
    div(s"page-list $cls")(
      el("h3").withText(title),
      ul(s"page-list $cls", pages, _.ref("sub"))
    )

  // TODO verify that it is a Directory!
  private lazy val directories: List[Page] = site
    .pages
    .filter(page => Directory.is(page.path))
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init)
    .sortBy(_.title)

  private lazy val pages: List[Page] = site
    .pages
    .filterNot(page => Directory.is(page.path))
    //    .filter(_.path.path.length > 0)
    .filter(_.path.path.init == path.path.init)
    .sortBy(_.title)

object Directory:
  val fileName: String = "index"

  def is(path: Path): Boolean = path.fileName == Directory.fileName

  // Implicitly force insertion of the missing `index` pages.
  def addParentDirectories(site: Site): Unit =
    site.pages.foreach(_.parent)

  def parent(site: Site, path: Path): Option[Directory] =
    val parentDirectory: Option[Seq[String]] =
      if Directory.is(path) && path.path.length > 1 then Some(path.path.init.init)
      else if !Directory.is(path) && path.path.nonEmpty then Some(path.path.init)
      else None
    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended(Directory.fileName) *).html
      site.getOrElse[Directory](parentPath):
        val parent = Directory(
          site,
          parentPath,
          frontMatter = FrontMatter.absent,
          pageMarkup = None
        )
        // Force insertion of the parent's parent for the newly-inserted parent
        parent.parent
        parent

  object Maker extends MarkupPage.Maker[Directory](Directory.apply):
    override def path(sourcePath: Path): Option[Path] = Option.when(Directory.is(sourcePath))(sourcePath)
