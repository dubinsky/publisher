package org.podval.tools.publish

import org.podval.tools.publish.util.Icon
import org.podval.xml.Html
import zio.blocks.html.*

final class Directory(site: Site, path: Path) extends MarkupPage(site, path):
  override protected def iconDefault: Icon = Icon.folder

  override def isSynthetic: Boolean = true
  override protected def syntheticContent: Option[Html.Element] = Some:
    div(className := "directory", Page.pageList(directories ++ pages))

  def prev(page: Page): Option[Page] = listFor(page).takeWhile(_ != page).reverse.headOption
  def next(page: Page): Option[Page] = listFor(page).dropWhile(_ != page).dropWhile(_ == page).headOption
  
  private def listFor(page: Page): List[Page] =
    if page.isInstanceOf[Directory]
    then directories
    else pages
    
  private lazy val directories: List[Page] = site
    .pages
    .filter(_.isDirectory)
    .filter(_.path.path.length > 1)
    .filter(_.path.path.init.init == path.path.init)
    .sortBy(_.title)

  private lazy val pages: List[Page] = site
    .pages
    .filterNot(_.isDirectory)
    .filter(_.path.path.init == path.path.init)
    .sortBy(_.title)

object Directory:
  val fileName: String = "index"

  def is(path: Path): Boolean = path.fileName == Directory.fileName

  // Implicitly force insertion of the missing `index` pages.
  def addParentDirectories(site: Site): Unit =
    site.pages.foreach(_.parent)

  def parent(site: Site, page: Page): Option[Directory] =
    val parentDirectory: Option[Seq[String]] =
      if page.isDirectory && page.path.path.length > 1 then Some(page.path.path.init.init)
      else if !page.isDirectory && page.path.path.nonEmpty then Some(page.path.path.init)
      else None

    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended(Directory.fileName) *).html
      site.find(parentPath)
        .map {
          case page: Directory => page
          case _ => throw IllegalArgumentException(s"Not a Directory")
        }
        .getOrElse:
          val parent: Directory = site.addPage(Directory(site, parentPath))
          // Force insertion of the parent's parent for the newly-inserted parent
          parent.parent
          parent
