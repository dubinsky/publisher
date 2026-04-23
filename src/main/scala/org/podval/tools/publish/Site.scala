package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File
import java.net.{URI, URISyntaxException}
import Util.ifDefined
import XmlUtil.{a, apply, childrenWhenEmpty, href, replaceAttribute}

final class Site(
  sourceDirectoryPath: String,
  treatWarningsAsErrors: Boolean,
  logLevel: Level = Level.INFO
):
  private given CanEqual[File, File] = CanEqual.derived
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)
  
  private var pagesVar: List[Page] = scala.compiletime.uninitialized
  def pages: List[Page] = pagesVar

  private val syntheticPages: List[SyntheticPage] = List(
    TagsPage(Path(List("tags")), this)
  )
  
  private var links: List[Link] = List.empty

  // TODO record warnings and generate report page(s):
  //- broken links
  //- inconsistent titles

  private def recover[A](z: => Either[PageError, A])(default: => A): Either[PageError, A] = z match
    case Right(value) => Right(value)
    case Left(error) => recover(error, default)

  private def recoverNone[A](z: => Either[PageError, A]): Either[PageError, Option[A]] = z match
    case Right(value) => Right(Some(value))
    case Left(error) => recover(error, None)

  // TODO use for nav items
  private def resolveRef(ref: String): Option[LinkResolved] = 
    LinkResolved.resolvePage(pages, ref).orElse(LinkResolved.resolveSyntheticPage(syntheticPages, ref))

  // TODO one backlink per page
  def backLinks(page: PageBase): List[Link] = links.filter(_.toPage == page).filterNot(_.from.page == page)

  def generateAndReport(): Unit = generate match
    case Right(()) => println("Done!")
    case Left(error) => println(s"Error generating site: $error")

  private def generate: Either[PageError, Unit] = for
    _ = Site.resourcesList.foreach: resourceName =>
      Files.write(
        toFile = File(config.targetDirectory, resourceName),
        content = Files.readResource(Site.resourcesBase + resourceName)
      )
      log.debug(s"Copied embedded asset: $resourceName")

    // Enumerate all pages
    pages: List[Page] <- directoryPages(List.empty, config.sourceDirectory)
    _ = this.pagesVar = pages

    // TODO add synthetic markup pages:
    //- tags.html
    //- sitemap.xml
    //- robots.txt
    //- feed.xml

    // Report conflicting pages
    _ <- recoverNone:
      pages.groupBy(_.targetPath).filter(_._2.length > 1).toList match
        case Nil => Right(Some(()))
        case duplicates => Util.sequence(duplicates)((targetPath: Path, pages: List[Page]) =>
          Left(PageError(
            pages.head.sourcePath,
            s"Duplicates for the target path $targetPath: ${pages.map(_.sourcePath).tail.mkString(", ")}"
          ))
        )

    // Resolve links
    // TODO do the warning.recover/sequence dance!
    // TODO sort the pages in transclusion order and transclude
    _ = pages.foreach(_.resolveLinks(this))

    // Write pages
    _ = (syntheticPages ++ pages).foreach: page =>
      Files.write(
        toFile = page.targetPath.file(config.targetDirectory),
        content = XmlUtil.write(Minima(config, page, backLinks(page)).render)
      )
      log.debug(s"Wrote: $page")

  yield ()
  
  private def recover[A](error: PageError, default: A): Either[PageError, A] =
    if treatWarningsAsErrors then Left(error) else
      // TODO collect all warnings with their kinds and sources
      // and generate reports for them!
      log.warn(s"Ignoring error: ${error.toString}")
      Right(default)

  private def directoryPages(path: List[String], directory: File): Either[PageError, List[Page]] =
    Files.requireExists(directory)
    Files.requireDirectory(directory)

    val (files: List[File], directories: List[File]) = Files
      .list(directory)
      .filterNot(config.exclude)
      .partition(_.isFile)

    for
      fromFiles: List[Option[Page]] <-
        Util.sequence(files)(filePage(path, _))
      fromDirectories: List[List[Page]] <-
        Util.sequence(directories)(directory => directoryPages(path :+ directory.getName, directory))
    yield
      fromFiles.flatten ++ fromDirectories.flatten

  private def filePage(path: List[String], file: File): Either[PageError, Option[Page]] =
    Files.requireExists(file)
    Files.requireFile(file)
    val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
    val sourcePath: Path = Path(path :+ name, extension)
    val result: Either[PageError, Option[Page]] = makePage(sourcePath)

    result match
      case Right(Some(page)) => log.debug(s"Read: $page")
      case _ =>

    result

  private def makePage(sourcePath: Path): Either[PageError, Option[Page]] =
    val pageKind: PageKind = PageKind(sourcePath, config)
    val sourceFile: File = sourcePath.file(config.sourceDirectory)
    val markup: Option[Markup] = sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension)))

    ifDefined(recoverNone(pageKind.targetPath(sourcePath))): (targetPath: Path) =>
      ifDefined(markup match
        case None =>
          ifDefined(recoverNone(
            if pageKind.isAssetAllowed
            then Right(())
            else Left(PageError(sourcePath, s"Asset not allowed in $pageKind"))
          )): _ =>
            Files.copy(fromFile = sourceFile, toFile = targetPath.file(config.targetDirectory))
            log.debug(s"Copied asset: $targetPath")
            Right(None)

        case Some(markup) =>
          ifDefined(recoverNone(
            if pageKind.isMarkupAllowed(markup)
            then Right(())
            else Left(PageError(sourcePath, s"Markup $markup not allowed in $pageKind"))
          )): _ =>
            val (frontMatterOrError: Either[PageError, FrontMatter], content: String) = FrontMatter
              .parse(Files.read(sourceFile)) match
                case (Right(frontMatter), content) =>
                  (Right(frontMatter), content)
                case (Left(yamlError), content) =>
                  (Left(PageError(sourcePath, "Malformed FrontMatter", Some(yamlError))), content)

            for
              frontMatter: FrontMatter <- recover(frontMatterOrError)(FrontMatter.absent)
              result: Option[Page] <- ifDefined(recoverNone(markup.parse(sourcePath, content))): xml =>
                Right(Some(Page(
                  sourcePath = sourcePath,
                  targetPath = targetPath.withExtension(Html.extension),
                  pageKind = pageKind,
                  markup = markup,
                  frontMatter = frontMatter,
                  xmlRaw = xml
                )))
            yield
              result
      ): (page: Page) =>
        Right(Some(page))

  // TODO mark errors with class attribute
  def resolveLink(from: Link.From): Xml.Element =
    def unresolved: Xml.Element = from.element.getOrElse(a("wiki-link", from.fromElement.ref.getOrElse("/"))())
      .childrenWhenEmpty(from.fromElement.text)
      
    from.fromElement.ref match
      case None => unresolved
      case Some(ref) => (if !from.transclude then None else Site.embedLink(ref, from.fromElement.text)).getOrElse:
        Site.externalRef(ref).fold(resolveRef(ref))(_ => None) match
          case None => unresolved
          case Some(linkResolved) =>
            // Register resolved link
            links = links.appended(Link(from, linkResolved.page))
  
            if from.transclude then a("transclude", linkResolved.url)(linkResolved.text) else
              val result: Xml.Element = from.element match
                case None => a("wiki-link", linkResolved.url)()
                case Some(element) => element.copy(name = a).replaceAttribute(href, XmlUtil.escapeUrl(linkResolved.url))
  
              result.childrenWhenEmpty(Some(linkResolved.text))

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
