package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.io.File
import java.time.LocalDate

sealed abstract class Page(
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

  def title: String

  protected def paths: List[Path]

  def write(): Unit

object Page:
  sealed trait WithContent extends Page:
    final override def write(): Unit = Files.write(targetFile, content)
    def content: String

  sealed trait WithXml extends WithContent:
    final override def content: String = XmlUtil.write(xmlContent)
    def xmlContent: Xml.Element
    def xml: Xml.Element // TODO move down?

  sealed trait WithoutFrontMatter extends Page:
    final override def title: String = path.title

  // TODO rename WithHtml?
  sealed trait WithFrontMatter extends Page with WithXml:
    def frontMatter: FrontMatter
    def localDate: Option[LocalDate]
    final override def title: String = frontMatter.title.getOrElse(path.title)
    final override def xmlContent: Xml.Element = Minima(this).render

  sealed trait WithoutSource extends Page:
    final override protected def paths: List[Path] = List(
      path,
      path.withoutExtension
    )

  sealed trait WithSource extends Page:
    def sourcePath: Path

    final override protected def paths: List[Path] = List(
      path,
      path.withoutExtension,
      sourcePath,
      sourcePath.withoutExtension
    )

  final class Asset(
    site: Site,
    path: Path
  ) extends Page(
    site,
    path
  ) with WithoutFrontMatter with WithSource:
    override def toString: String = s"Asset $path"
    override def sourcePath: Path = path
    override def write(): Unit = Files.copy(fromFile = sourcePath.file(site.sourceDirectory), toFile = targetFile)

  final class Resource(
    site: Site,
    path: Path
  ) extends Page(
    site,
    path
  ) with WithoutFrontMatter with WithoutSource with WithContent:
    override def toString: String = s"Resource $path"
    override def content: String = Files.readResource(Site.resourcesBase + path.toString)

  abstract class SyntheticText(
    site: Site,
    path: Path
  ) extends Page(
    site,
    path
  ) with WithoutFrontMatter with WithoutSource with WithContent:
    override def toString: String = s"SyntheticText $path"

  abstract class SyntheticXml(
    site: Site,
    path: Path
  ) extends Page(
    site,
    path
  ) with WithoutFrontMatter with WithoutSource with WithXml:
    override def toString: String = s"SyntheticXml $path"
    final override def xmlContent: Xml.Element = xml

  // TODO introduce layout-like pages that both parse from existing file *and* add content;
  // use them for the home index - and others?

  abstract class SyntheticMarkupPage(
    site: Site,
    path: Path,
    override val frontMatter: FrontMatter
  ) extends Page(
    site,
    path
  ) with WithFrontMatter with WithoutSource:
    override def toString: String = s"SyntheticMarkupPage $path"
    final override def localDate: Option[LocalDate] = frontMatter.date.map(_.localDate)

  abstract class MarkupPage(
    site: Site,
    path: Path,
    override val sourcePath: Path,
    markup: Markup,
    postDate: Option[LocalDate],
    override val frontMatter: FrontMatter,
    xmlRaw: Xml.Element
  ) extends Page(
    site,
    path
  ) with WithFrontMatter with WithSource:
    override def toString: String =
      val source: String = if sourcePath.withoutExtension == path.withoutExtension then "" else s" ($sourcePath)"
      s"MarkupPage $path$source"

    protected var xmlVar: Xml.Element = xmlRaw
    final override def xml: Xml.Element = xmlVar

    def isPost: Boolean = postDate.isDefined || frontMatter.layout.contains("post")
    final override def localDate: Option[LocalDate] = postDate.orElse(frontMatter.date.map(_.localDate))
