package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File
import java.net.{URI, URISyntaxException}
import XmlUtil.{a, apply, childrenWhenEmpty}

final class Site(
  sourceDirectoryPath: String,
  production: Boolean,
  targetDirectoryName: String,
  includeDrafts: Boolean,
  treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

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

  val socialLinks: Seq[SocialLink] = Seq(
    config.social.github.map(SocialLink.GitHub(_)),
    config.social.twitter.map(SocialLink.Twitter(_)),
    config.social.linkedin.map(SocialLink.LinkedIn(_))
  ).flatten

  private val relocator: Relocator = Relocator(
    config.postsDirectoryName,
    config.draftsDirectoryName,
    config.dailyNotesDirectoryName
  )

  private var links: List[Link] = List.empty

  val tags: Tags = Tags(this, Path("tags").withExtension(Html.extension))
  private val sitemap: Sitemap = Sitemap(this, Path("sitemap").withExtension("xml"))

  private var pagesVar: List[Page] =
    // Embedded resources
    Site.resourcesList.map(Page.Resource(this, _)) ++
    // Synthetics
    List(
      tags,
      Errors(this, Path("errors").withExtension(Html.extension)),
      Feed(this, Path("feed").withExtension("xml")),
      sitemap,
      Robots(this, Path("robots").withExtension("txt"), sitemap)
    )

  def pages: List[Page] = pagesVar
  def pagesWithFrontMatter: List[Page.WithFrontMatter] = pages.collect { case page: Page.WithFrontMatter => page }
  def markupPages: List[MarkupPage] = pages.collect{ case page: MarkupPage => page }

  private var headerPagesVar: List[Link.ToPage] = scala.compiletime.uninitialized
  def headerPages: List[Link.ToPage] = headerPagesVar

  private var errorsVar: List[PageError] = List.empty
  def errors: List[PageError] = errorsVar

  def reportError[R](error: PageError, result: R): R =
    if !treatErrorsAsWarnings then throw error else
      errorsVar = errorsVar.appended(error)
      log.warn(error.getMessage)
      result

  def backLinks(page: Page): List[Link] = links
    .filter(_.to.page == page)
    .filterNot(_.from.page == page)
    .distinctBy(_.from.page.path) // TODO once we have context, each link should be listed (grouped by page)

  def posts: List[Page.WithFrontMatter] = pagesWithFrontMatter
    .filter(page => page.frontMatter.layout.contains("post") && page.frontMatter.date.isDefined) // TODO
    .sortBy(_.frontMatter.date.get)
    .reverse

  def subDirectories(page: Page): List[Page] = if !page.path.isIndex then List.empty else pages
    .filter(_.path.isIndex)
    .filter(_.path.path.init.init == page.path.path.init)
    .sortBy(_.title)

  def subPages(page: Page): List[Page] = if !page.path.isIndex then List.empty else pages
    .filter(_.path.path.init == page.path.path.init)
    .filterNot(_ == page)
    .sortBy(_.title)

  def generateAndReport(): Unit =
    try
      generate()
      println("Done!")
    catch case error: PageError =>
      println(s"Error generating site: ${error.getMessage}")

  private def generate(): Unit =
    // Wipe out output directory
    Files.deleteDirectory(config.targetDirectory)

    // Read pages
    pagesVar = pagesVar ++ directoryPages(Seq.empty, config.sourceDirectory)

    // TODO calculate parent and children for pages
    // TODO add missing index pages

    // Report conflicting pages
    pages
      .groupBy(_.path)
      .filter(_._2.length > 1)
      .toList
      .foreach((path: Path, pages: List[Page]) =>
        reportError(PageError.Duplicate(
          path,
          s"Duplicates for the path $path: ${pages.map(_.title).tail.mkString(", ")}"
        ), ())
      )

    // Resolve header pages
    headerPagesVar = config.headerPages.flatMap(resolveHeaderPage)

    // Resolve links
    markupPages.foreach(_.resolveLinks())

    // TODO sort the pages in transclusion order and transclude

    // TODO add TOCs

    // Write pages
    pages.foreach: page =>
      page.write()
      log.debug(s"Wrote: $page")

  private def directoryPages(directoryPath: Seq[String], directory: File): List[Page] =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    files.map(filePage(directoryPath, _)) ++
    directories.flatMap(directory => directoryPages(directoryPath :+ directory.getName, directory))

  private def filePage(directoryPath: Seq[String], file: File): Page.WithSource =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(directoryPath :+ name, extension)

    val page: Page.WithSource =
      sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
        case None =>
          Page.Asset(
            site = this,
            path = sourcePath
          )
        case Some(markup) =>
          val path: Path = relocator.relocate(sourcePath) match
            case Right(path) => path
            case Left(error) => reportError(error, sourcePath)

          MarkupPage(
            site = this,
            path = path.withExtension(Html.extension),
            sourcePath = sourcePath,
            markup = markup
          )

    log.debug(s"Read: $page")
    page

  private def resolveHeaderPage(ref: String): Option[Link.ToPage] =
    val result: Option[Link.ToPage] = resolveRef(ref).flatMap {
      case page: Link.ToPage => Some(page)
      case linkResolved => reportError(PageError.Unresolved(Path.root, s"Header page $ref is not a page: $linkResolved"), None)
    }
    if result.isDefined
    then result
    else reportError(PageError.Unresolved(Path.root, s"Header page $ref did not resolve"), result)

  private def resolveRef(ref: String): Option[Link.To] = Link.resolveRef(ref, this)

  // TODO mark errors with class attribute
  def resolveLink(from: Link.From): Xml.Element =
    val ref: String = from.fromElement.ref
    (if !from.transclude then None else Site.embedLink(ref, from.fromElement.text)).getOrElse:
      Site.externalRef(ref).fold(resolveRef(ref))(_ => None) match
        case None => from.element.getOrElse(a("wiki-link", from.fromElement.ref)())
          .childrenWhenEmpty(from.fromElement.text)
        case Some(linkTo) =>
          // Register resolved link
          links = links.appended(Link(from, linkTo))

          if from.transclude then linkTo.a("transclude") else from.element match
            case None => linkTo.a("wiki-link")
            case Some(element) => linkTo.a(element)

object Site:
  def main(args: Array[String]): Unit = Cli.main(Array("/home/dub/Podval/dub.podval.org"))

  val resourcesBase: String = "/org/podval/tools/publish/site"

  // TODO list using Files.listResources
  private val resourcesList: List[Path] = List(
    Path("assets", "css", "base").withExtension("css"),
    Path("assets", "css",  "initialize").withExtension("css"),
    Path("assets", "css",  "layout").withExtension("css"),
    Path("assets", "css",  "skin").withExtension("css"),
    Path("assets", "css",  "style").withExtension("css"),
  )

  // see https://obsidian.md/help/embeds
  private def embedLink(ref: String, text: Option[String]): Option[Xml.Element] =
    Files.nameAndExtension(ref)._2.flatMap: extension =>
      if Files.imageExtensions.contains(extension) then None
      // Embed image
      //      else if Files.audioExtensions.contains(extension) then
      //        // Embed audio player
      //      else if extension == "pdf" then
      //        // Embed PDF viewer
      else None

  private def externalRef(ref: String): Option[URI] =
    if ref.contains(" ") then None else
      try
        val uri: URI = URI(ref)
        // TODO recognize and resolve links to *this* site
        Option.when(uri.getScheme != null)(uri)
      catch
        case e: URISyntaxException =>
          // TODO handle errors better - and log them
          println(s"Malformed URL: $ref $e")
          None
