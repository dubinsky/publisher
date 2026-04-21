package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import zio.blocks.schema.xml.Xml
import java.io.File
import Util.ifDefined

final class Site(
  sourceDirectoryPath: String,
  treatWarningsAsErrors: Boolean,
  logLevel: Level = Level.INFO
):
  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)

  private val warnings: Warnings = Warnings(treatWarningsAsErrors = true)

  private given CanEqual[File, File] = CanEqual.derived

  def generateAndReport(): Unit = generate match
      case Right(()) => println("Done!")
      case Left(error) => println(s"Error generating site: $error")

  private val resourcesBase: String = "/org/podval/tools/publish/site"

  // TODO list using Files.listResources
  private val resourcesList: List[String] = List(
    "/assets/css/base.css",
    "/assets/css/initialize.css",
    "/assets/css/layout.css",
    "/assets/css/skin.css",
    "/assets/css/style.css",
  )

  private def generate: Either[PageError, Unit] = for
    _ = resourcesList.foreach: resourceName =>
      Files.write(
        toFile = File(config.targetDirectory, resourceName),
        content = Files.readResource(resourcesBase + resourceName)
      )
      log.debug(s"Copied embedded asset: $resourceName")

    // Enumerate all pages
    pages: List[Page] <- directoryPages(List.empty, config.sourceDirectory)

    // TODO add synthetic markup pages:
    //- tags.html
    //- sitemap.xml
    //- robots.txt
    //- feed.xml

    // Report conflicting pages
    _ <- warnings.recoverNone:
      pages.groupBy(_.targetPath).filter(_._2.length > 1).toList match
        case Nil => Right(Some(()))
        case duplicates => Util.sequence(duplicates)((targetPath: Path, pages: List[Page]) =>
          Left(PageError(
            pages.head.sourcePath,
            s"Duplicates for the target path $targetPath: ${pages.map(_.sourcePath).tail.mkString(", ")}"
          ))
        )

    links: Links = Links(pages, warnings)

    // Resolve links
    // TODO do the warning.recover/sequence dance!
    _ = pages.foreach(links.resolve)

    // Write pages
    _ = pages.foreach: page =>
      Files.write(
        toFile = page.targetPath.file(config.targetDirectory),
        content = XmlUtil.write(Minima(config, page, links.backLinks(page)).render)
      )
      log.debug(s"Wrote: $page")

  yield ()
  
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

    ifDefined(warnings.recoverNone(pageKind.targetPath(sourcePath))): (targetPath: Path) =>
      ifDefined(markup match
        case None =>
          ifDefined(warnings.recoverNone(
            if pageKind.isAssetAllowed
            then Right(())
            else Left(PageError(sourcePath, s"Asset not allowed in $pageKind"))
          )): _ =>
            Files.copy(fromFile = sourceFile, toFile = targetPath.file(config.targetDirectory))
            log.debug(s"Copied asset: $targetPath")
            Right(None)

        case Some(markup) =>
          ifDefined(warnings.recoverNone(
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
              frontMatter: FrontMatter <- warnings.recover(frontMatterOrError)(FrontMatter.absent)
              result: Option[Page] <- ifDefined(warnings.recoverNone(markup.parse(sourcePath, content))): xml =>
                val (xmlWithAnchors: Xml.Element, toc: Toc) = Toc(xml) // TODO different for TEI...
                Right(Some(Page(
                  sourcePath = sourcePath,
                  targetPath = targetPath.withExtension(Html.extension),
                  pageKind = pageKind,
                  markup = markup,
                  frontMatter = frontMatter,
                  xml = xmlWithAnchors,
                  toc = toc
                )))
            yield
              result
      ): (page: Page) =>
        Right(Some(page))

