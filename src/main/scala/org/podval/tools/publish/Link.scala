package org.podval.tools.publish

import scala.annotation.tailrec

final class Link(
  val page: Page,
  fragment: Option[Link.ToFragment] = None
):
  def url: String = withFragment(page.path.toString /* TODO URL-encode?*/, _.id)
  def title: String = withFragment(page.title, _.title)

  private def withFragment(
    fromPage: String,
    get: Link.ToFragment => String
  ): String = fromPage + fragment.fold("")(fragment => s"#${get(fragment)}")

object Link:
  sealed abstract class ToFragment:
    def title: String
    def id: String

  final class ToBlock(block: Page.Block) extends ToFragment:
    override def id: String = block.id
    override def title: String = s"^${block.id}"

  final class ToSection(sections: Seq[Page.Section]) extends ToFragment:
    override def id: String = sections.last.id
    override def title: String = sections.map(_.title).mkString("#")

  final class ToId(override val id: String) extends ToFragment:
    override def title: String = s"#$id"

  // path could be `name`, `path/name`(?) - or empty, for intrapage links.
  // fragment could be `#section`, `#section#subsection`, `#^block`, or #id.
  def resolve(ref: String, page: Page): Option[Link] =
    val (pathString: String, fragment: Option[String]) = Strings.split(ref, '#')

    val toPage: Option[Page] = if pathString.trim.isEmpty then Some(page) else
      val isAbsolute: Boolean = pathString.trim.startsWith("/")
      val pathSegments: Seq[String] = pathString.trim.split('/').toSeq.filterNot(_.isEmpty).map(_.trim)
      val path: Path = if pathSegments.isEmpty then Path.root else
        val (lastSegment, extension) = Files.nameAndExtension(pathSegments.last)
        Path(pathSegments.init :+ lastSegment.trim, extension)

      page.site.pages.find(page => is(page, path, isAbsolute))

    toPage
      .map(page => Link(page, fragment = fragment.flatMap: fragment =>
        if fragment.startsWith("^")
        then page.resolveBlock(id = fragment.substring(1).trim)
        else page.resolveSection(names = fragment.split('#').map(_.trim).toSeq)
      ))
      .orElse(fragment.map(resolveId(page, _)))

  def resolveId(page: Page, id: String) = Link(page, Some(page.resolveId(id).get))

  private def is(page: Page, path: Path, isAbsolute: Boolean): Boolean =
    isPath(page, path, isAbsolute) ||
    page.sourcePathOpt.exists(isSourcePath(_, path, isAbsolute))

  private def isPath(page: Page, path: Path, isAbsolute: Boolean): Boolean =
    @tailrec
    def loop(current: Page, path: Seq[String]): Boolean =
      val name: String = path.head
      val is: Boolean =
        current.fileName == name ||
        current.title == name ||
        current.aliases.contains(name)

      if !is then false else current.parent match
        case None =>
          path.tail.isEmpty
        case Some(parent) =>
          if path.tail.isEmpty
          then !isAbsolute
          else loop(parent, path.tail)

    isExtension(page.path, path) && loop(page, path.path)

  private def isSourcePath(sourcePath: Path, path: Path, isAbsolute: Boolean): Boolean =
    isExtension(sourcePath, path) && (
      if isAbsolute
      then sourcePath.path == path.path
      else sourcePath.path.endsWith(path.path)
    )

  private def isExtension(pagePath: Path, path: Path): Boolean =
    path.extension.fold(true)(pagePath.extension.contains)
