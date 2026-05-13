package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import scala.reflect.TypeTest
import java.io.File

final class Site(
  sourceDirectoryPath: String,
  production: Boolean,
  targetDirectoryName: String,
  includeDrafts: Boolean,
  treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)
  val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(
    sourceDirectoryPath,
    targetDirectoryName,
    includeDrafts
  )

  def sourceDirectory: File = config.sourceDirectory
  def targetDirectory: File = config.targetDirectory

  def title: String = config.title
  def description: String = config.description
  def url: String = config.url
  def author: String = config.author
  def email: String = config.email
  def lang: String = config.lang.getOrElse("en")
  def googleAnalytics: Option[String] = Option.when(production)(config.googleAnalytics).flatten

  private var pagesVar: List[Page] = List.empty
  def pages: List[Page] = pagesVar

  // TODO make a lazy val
  def markupPages: List[MarkupPage] = pages.collect { case page: MarkupPage => page }

  def getOrElse[P <: MarkupPage](path: Path)(make: => P)(using TypeTest[MarkupPage, P]): P = markupPages
    .find(_.path == path)
    .map {
      case page: P => page
      case _ => throw IllegalArgumentException(s"Not a ???") // TODO use some tag to print the class name
    }
    .getOrElse:
      val page = make
      log.debug(s"Added $page")
      pagesVar = pagesVar.appended(page)
      page

  private val pageMakers: Seq[MarkupPage.Maker[?]] = Seq(
    Post.Maker(
      site = this,
      postsDirectoryName = config.postsDirectoryName,
      draftsDirectoryName = config.draftsDirectoryName,
      dailyNotesDirectoryName = config.dailyNotesDirectoryName
    ),
    Directory.Maker,
    Tags.Maker,
    Errors.Maker,
    Posts.Maker,
    MarkupPage.DefaultMaker
  )

  private var errorsVar: List[PageError] = List.empty
  def errors: List[PageError] = errorsVar

  def reportError[R](error: PageError, result: R): R =
    if !treatErrorsAsWarnings then throw error else
      errorsVar = errorsVar.appended(error)
      log.warn(error.getMessage)
      result

  def generate(): Unit =
    // Wipe out output directory
    Files.deleteDirectory(config.targetDirectory)

    // Write embedded resources
    writePages(Site.resourcesList.map(Asset.Embedded(this, _)))

    // Write synthetic assets
    writePages(List(Sitemap(this), Robots(this), Feed(this)))

    // Read and add pages
    pagesVar = pagesVar.appendedAll(directoryPages(Seq.empty, config.sourceDirectory))

    // Add automatic pages
    pageMakers.collect { case autoMaker: MarkupPage.AutoMaker[?] => autoMaker }.foreach(_.get(this))

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) =>
        PageError.Duplicate(
          path,
          s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
        ).report(this, ())
      )

    // Add directory pages
    Directory.addParentDirectories(this)

    // Gather back-links
    markupPages.foreach(page => backLinks.addBackLinks(page.backLinks))

    // TODO sort pages topologically based on transclusions

    // Write pages
    writePages(pages)

    // Done
    log.info("Done!")

  private def directoryPages(directoryPath: Seq[String], directory: File): List[Page] =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    files.map(filePage(directoryPath, _)) ++
    directories.flatMap(directory => directoryPages(directoryPath :+ directory.getName, directory))

  private def filePage(directoryPath: Seq[String], file: File): Page =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(directoryPath :+ name, extension)

    val page: Page = sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
      case None => Asset.WithSource(site = this, path = sourcePath)

      case Some(markup) =>
        val (frontMatter: FrontMatter, xml: Xml.Element) = parseMarkup(sourcePath, markup)
        val source: MarkupPage.Source = MarkupPage.Source(markup, this, sourcePath)
        source.cache(xml)

        pageMakers
          .flatMap(_.withSource(this, frontMatter, source))
          .headOption
          .getOrElse(throw PageError.Unmakable(sourcePath, s"Can't make the page!"))

    log.debug(s"Read: $page")
    page

  def parseMarkup(sourcePath: Path, markup: Markup): (FrontMatter, Xml.Element) =
    val (frontMatterOrError: Either[PageError, FrontMatter], markupContent: String) =
      FrontMatter.parse(sourcePath, Files.read(sourcePath.file(sourceDirectory)))

    val frontMatter: FrontMatter = frontMatterOrError match
      case Right(frontMatter) => frontMatter
      case Left(error) => error.report(this, FrontMatter.absent)

    val xml: Xml.Element = markup.parse(sourcePath, markupContent) match
      case Right(xml) => xml
      case Left(error) =>
        var result = Xml.element("div")
        result = Xml.ClassName.add(result, "malformed-xml")
        result = Xml.setText(result, s"Malformed XML: $error")
        error.report(this, result)

    (frontMatter, xml)

  private def writePages(pages: List[Page]): Unit = pages.foreach: page =>
    page.write()
    log.debug(s"Wrote: $page")

  lazy val tags: Tags = Tags.Maker.get(this)

  private val backLinks: BackLinks = BackLinks()
  def backLinks(page: Page): Seq[(MarkupPage, List[BackLinks.BackLink])] = backLinks.backLinks(page)

  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  lazy val headerPages: List[Site.HeaderPage] = markupPages.flatMap(_.headerPage).sortBy(_.priority)

object Site:
  final class HeaderPage(
    val page: MarkupPage,
    val priority: Int
  )

  def main(args: Array[String]): Unit = Cli.main(Array(
    "--log-level=INFO",
//    "--treat-errors-as-warnings=true",
    "/home/dub/Podval/dub.podval.org"
  ))

  val mainStyleSheet: String = "/assets/css/style.css"

  // TODO list using Files.listResources
  val resourcesBase: String = "/org/podval/tools/publish/site"
  private val resourcesList: List[Path] = List(
    Path("assets", "css", "base").withExtension("css"),
    Path("assets", "css",  "initialize").withExtension("css"),
    Path("assets", "css",  "layout").withExtension("css"),
    Path("assets", "css",  "skin").withExtension("css"),
    Path("assets", "css",  "style").withExtension("css"),
  )
