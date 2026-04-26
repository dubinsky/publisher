package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File
import java.net.{URI, URISyntaxException}
import XmlUtil.{a, apply, childrenWhenEmpty}

final class Site(
  sourceDirectoryPath: String,
  treatErrorsAsWarnings: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)

  private val locators: List[Locator] =
    List(Locator.BlogPost(config.blogDirectoryName)) ++
    config.dailyNotesDirectoryName.map(Locator.DailyNote(_)).toList

  private var links: List[Link] = List.empty

  private var pagesVar: List[PageBase] = List(
    TagsReport(Path(List("tags")), this),
    ErrorsReport(Path(List("errors")), this),
  )
  
  def pages: List[PageBase] = pagesVar

  private var headerPagesVar: List[Link.ToPage] = scala.compiletime.uninitialized
  def headerPages: List[Link.ToPage] = headerPagesVar

  private var errorsVar: List[PageError] = List.empty
  def errors: List[PageError] = errorsVar

  private def reportError(error: PageError): Unit = if !treatErrorsAsWarnings then throw error else
    errorsVar = errorsVar.appended(error)
    log.warn(error.getMessage)

  private def recoverNone[A](z: => Either[PageError, A]): Option[A] = z match
    case Right(value) => Some(value)
    case Left(error) =>
      reportError(error)
      None

  def title: String = config.title
  def description: String = config.description
  def author: String = config.author
  def email: String = config.email
  def lang: String = config.lang.getOrElse("en")

  def backLinks(page: PageBase): List[Link] = links
    .filter(_.to.page == page)
    .filterNot(_.from.page == page)
    .distinctBy(_.from.page.targetPath)

  def tags: List[String] = pages.flatMap(_.frontMatter.tags).distinct.sorted

  def withTag(tag: String): List[PageBase] = pages.filter(_.frontMatter.tags.contains(tag)).sortBy(_.title)

  def posts: List[PageBase] = pages
    .filter(page => page.frontMatter.layout.contains("post") && page.frontMatter.date.isDefined) // TODO
    .sortBy(_.frontMatter.date.get)
    .reverse

  def subDirectories(page: PageBase): List[PageBase] = if !page.targetPath.isIndex then List.empty else pages
    .filter(_.targetPath.isIndex)
    .filter(_.targetPath.path.init.init == page.targetPath.path.init)
    .sortBy(_.title)

  def subPages(page: PageBase): List[PageBase] = if !page.targetPath.isIndex then List.empty else pages
    .filter(_.targetPath.path.init == page.targetPath.path.init)
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

    // Write embedded resources
    Site.resourcesList.foreach: resourceName =>
      Files.write(
        toFile = File(config.targetDirectory, resourceName),
        content = Files.readResource(Site.resourcesBase + resourceName)
      )
      log.debug(s"Copied embedded asset: $resourceName")

    // Enumerate all pages
    pagesVar = pagesVar ++ directoryPages(List.empty, config.sourceDirectory)

    // TODO add missing index pages
    // TODO write:
    //- sitemap.xml
    //- robots.txt
    //- feed.xml

    // Report conflicting pages
    pages
      .groupBy(_.targetPath)
      .filter(_._2.length > 1)
      .toList
      .foreach((targetPath: Path, pages: List[PageBase]) =>
        reportError(PageError.Duplicate(
          targetPath,
          s"Duplicates for the target path $targetPath: ${pages.map(_.title).tail.mkString(", ")}"
        ))
      )

    // Resolve links
    pages.collect{ case page: Page => page }.foreach(_.resolveLinks(this))

    // TODO sort the pages in transclusion order and transclude

    headerPagesVar = config.headerPages.flatMap(resolveHeaderPage)

    // Write pages
    pages.foreach: page =>
      Files.write(
        toFile = page.targetPath.file(config.targetDirectory),
        content = XmlUtil.write(Minima(this, page).render)
      )
      log.debug(s"Wrote: $page")

  private def directoryPages(path: List[String], directory: File): List[Page] =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    files.flatMap(filePage(path, _)) ++
    directories.flatMap(directory => directoryPages(path :+ directory.getName, directory))

  private def filePage(path: List[String], file: File): Option[Page] =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(path :+ name, extension)
    val sourceFile: File = sourcePath.file(config.sourceDirectory)
    val locator: Option[Locator] = locators.find(_.is(sourcePath))
    val targetPath: Option[Path] = locator match
      case None => Some(sourcePath)
      case Some(locator) => recoverNone(locator.targetPath(sourcePath))

    targetPath.flatMap: (targetPath: Path) =>
      sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
        case None =>
          if !locator.fold(true)(_.isAssetAllowed)
          then
            reportError(PageError.FileKind(sourcePath, s"Asset not allowed in $locator"))
          else
            Files.copy(fromFile = sourceFile, toFile = targetPath.file(config.targetDirectory))
            log.debug(s"Copied asset: $targetPath")
          None

        case Some(markup) =>
          if !locator.fold(true)(_.isMarkupAllowed(markup))
          then
            reportError(PageError.FileKind(sourcePath, s"Markup $markup not allowed in $locator"))
            None
          else
            val (frontMatterOrError: Either[PageError, FrontMatter], content: String) =
              FrontMatter.parse(sourcePath, Files.read(sourceFile))

            val frontMatter: FrontMatter = frontMatterOrError match
              case Right(value) => value
              case Left(error) =>
                reportError(error)
                FrontMatter.absent

            recoverNone(markup.parse(sourcePath, content)).map: xml =>
              val page = Page(
                sourcePath = sourcePath,
                targetPath = targetPath.withExtension(Html.extension),
                markup = markup,
                frontMatter = frontMatter,
                xmlRaw = xml
              )
              log.debug(s"Read: $page")
              page

  private def resolveHeaderPage(ref: String): Option[Link.ToPage] =
    val result = resolveRef(ref).flatMap {
      case page: Link.ToPage => Some(page)
      case linkResolved =>
        reportError(PageError.Unresolved(Path.root, s"Header page $ref resolved not to a page: $linkResolved"))
        None
    }
    if result.isEmpty then reportError(PageError.Unresolved(Path.root, s"Header page $ref did not resolve"))
    result

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
  private val resourcesBase: String = "/org/podval/tools/publish/site"

  // TODO list using Files.listResources
  private val resourcesList: List[String] = List(
    "/assets/css/base.css",
    "/assets/css/initialize.css",
    "/assets/css/layout.css",
    "/assets/css/skin.css",
    "/assets/css/style.css",
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
