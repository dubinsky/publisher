package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.time.LocalDate

abstract class MarkupPage(
  site: Site,
  path: Path
) extends Page(
  site,
  path
) with Page.WithXmlContent:
  final override def xmlContent: Xml.Element =
    postProcess()
    Minima(this).render

  // add TOC, remove title etc.
  protected def postProcess(): Unit

  final def isPost: Boolean = postDate.isDefined || frontMatter.layout.contains("post")

  final def date: Option[Date] = postDate.map(Date.Local(_)).orElse(frontMatter.date)

  final def author: Option[String] = frontMatter.author // TODO default to site.author?

  def postDate: Option[LocalDate]

  def frontMatter: FrontMatter

  def xml: Xml.Element

  def toc: Toc

  def resolveLinks(): Unit

  final def isIndex: Boolean = path.fileName == "index" && path.path.length > 1 // TODO "home" is not an index

  final override def title: String = frontMatter.title.getOrElse:
    if !isIndex
    then path.fileName
    else path.path.init.last

  final lazy val parent: Option[Page] =
    // Note: top-level `index` is no-one's parent.
    val parentDirectory: Option[Seq[String]] =
      if path.fileName == "index" && path.path.length > 2 then Some(path.path.init.init)
      else if path.fileName != "index" && path.path.length > 1 then Some(path.path.init)
      else None
    parentDirectory.map: parentDirectory =>
      val parentPath: Path = Path(parentDirectory.appended("index") *).withExtension(Html.extension)
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

object MarkupPage:
  final class WithSource(
    site: Site,
    path: Path,
    override val sourcePath: Path,
    markup: Markup,
    override val postDate: Option[LocalDate],
    override val frontMatter: FrontMatter,
    xmlRaw: Xml.Element
  ) extends MarkupPage(
    site = site,
    path = path
  ) with Page.WithSource:
    private var xmlVar: Xml.Element = xmlRaw

    override def xml: Xml.Element = xmlVar

    override lazy val toc: Toc =
      xmlVar = markup.dropAnchors(xml)
      Toc(markup.sections(xml))

    override def resolveLinks(): Unit =
      xmlVar = markup.resolveLinks(xml, this)

    override protected def postProcess(): Unit =
      xmlVar = markup.addToc(xml, toc)

  abstract class Synthetic(
    site: Site,
    path: Path,
    final override val frontMatter: FrontMatter
  ) extends MarkupPage(
    site,
    path
  ) with Page.WithoutSource:
    final override def postDate: Option[LocalDate] = None
    final override def toc: Toc = Toc.empty
    final override def resolveLinks(): Unit = ()
    final override def postProcess(): Unit = ()

  final class Index(
    site: Site,
    path: Path
  ) extends Synthetic(
    site,
    path,
    FrontMatter.absent
  ):
    override def xml: Xml.Element = XmlUtil.div("empty-content").build

  def apply(
    site: Site,
    path: Path,
    sourcePath: Path,
    markup: Markup,
    postDate: Option[LocalDate] // if the page is a post or a daily note
  ): WithSource =
    val (frontMatterOrError: Either[PageError, FrontMatter], markupContent: String) =
      FrontMatter.parse(sourcePath, Files.read(sourcePath.file(site.sourceDirectory)))

    new WithSource(
      site = site,
      path = path.withExtension(Html.extension),
      sourcePath = sourcePath,
      markup = markup,
      postDate = postDate,
      frontMatter = frontMatterOrError match
        case Right(frontMatter) => frontMatter
        case Left(error) => site.reportError(error, FrontMatter.absent),
      xmlRaw = markup.parse(sourcePath, markupContent) match
        case Right(xml) => xml
        case Left(error) => site.reportError(error, Xml.Element("div", Xml.Text(s"Malformed XML: $error")))
    )
