package org.podval.tools.publish

sealed abstract class LinkResolved(val page: PageBase):
  def url: String

  def text: String

object LinkResolved:
  private final case class ToPage(override val page: PageBase) extends LinkResolved(page):
    override def url: String = page.targetPath.toString
    override def text: String = page.title

  private final case class ToSection(override val page: Page, sections: Seq[Section]) extends LinkResolved(page):
    override def url: String = s"${page.targetPath.toString}#${sections.last.id}"
    override def text: String = s"${page.title}#${sections.map(_.title).mkString("#")}"

  private final case class ToBlock(override val page: Page, block: Block) extends LinkResolved(page):
    override def url: String = s"${page.targetPath.toString}#${block.id}"
    override def text: String = s"${page.title}#^${block.id}"

  // TODO move into Site while keeping syntax here
  // TODO register warnings
  def resolvePage(pages: List[Page], ref: String): Option[LinkResolved] =
    val (toPath: String, fragment: Option[String]) = Files.split(ref, '#')

    pages.find(_.is(toPath)).flatMap: toPage =>
      fragment match
        case None =>
          Some(LinkResolved.ToPage(toPage))
        case Some(fragment) =>
          if fragment.startsWith("^") then
            for
              id: String = fragment.substring(1).trim
              block: Block <- toPage.block(id)
            yield LinkResolved.ToBlock(toPage, block)
          else
            for
              names: Seq[String] = fragment.split('#').map(_.trim).toSeq
              sections: Seq[Section] <- toPage.section(names)
            yield LinkResolved.ToSection(toPage, sections)

  def resolveSyntheticPage(pages: List[SyntheticPage], ref: String): Option[LinkResolved] =
    val (toPath: String, fragment: Option[String]) = Files.split(ref, '#')
    // TODO warning if fragment.nonEmpty

    pages.find(_.is(toPath)).flatMap(toPage => Some(LinkResolved.ToPage(toPage)))
