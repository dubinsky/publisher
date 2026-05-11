package org.podval.tools.publish

final class Link(
  val page: Page, 
  fragment: Option[Link.ToFragment] = None
):
  def url: String = withFragment(page.path.toString /* TODO URL-encode!*/, _.id)
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

  // path could be `name`, `path/name`(?)
  // fragment could be `#section`, `#section#subsection`, `#^block`
  def resolve(ref: String, site: Site): Option[Link] =
    val (pathString: String, fragment: Option[String]) = Strings.split(ref, '#')
    val isAbsolute: Boolean = pathString.trim.startsWith("/")
    val pathSegments: Seq[String] = pathString.trim.split('/').toSeq.filterNot(_.isEmpty).map(_.trim)
    val path: Path = if pathSegments.isEmpty then Path.root else
      val (lastSegment, extension) = Files.nameAndExtension(pathSegments.last)
      Path(pathSegments.init :+ lastSegment.trim, extension)

    if path.path.isEmpty then None else site
      .pages
      .find(page =>
        page.path.is(path, isAbsolute) ||
        page.sourcePathOpt.exists(_.is(path, isAbsolute))
      )
      .map(page => Link(page, fragment = fragment.flatMap: fragment =>
        if fragment.startsWith("^")
        then page.resolveBlock(id = fragment.substring(1).trim)
        else page.resolveSection(names = fragment.split('#').map(_.trim).toSeq)
      ))
