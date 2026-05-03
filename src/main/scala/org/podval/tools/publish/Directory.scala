package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final class Directory(
  site: Site,
  path: Path,
  pageMarkup: Option[PageMarkup],
  frontMatter: FrontMatter
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup
):
  // TODO verify that it is a Directory!
  lazy val directories: List[Page] = site
    .pages
    .filter(page => Directory.is(page.path))
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init)
    .sortBy(_.title)

  lazy val pages: List[Page] = site
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
    site.markupPages.foreach(_.parent)

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

  final class Maker(
    site: Site
  ) extends MarkupPage.Maker(site):

    override def withSource(
      sourcePath: Path,
      markup: Markup,
      frontMatter: FrontMatter,
      xml: Xml.Element
    ): Option[MarkupPage] = Option.when(Directory.is(sourcePath)):
      Directory(
        site = site,
        path = sourcePath.html,
        pageMarkup = Some(PageMarkup(
          sourcePath,
          markup,
          xml
        )),
        frontMatter = frontMatter
      )

