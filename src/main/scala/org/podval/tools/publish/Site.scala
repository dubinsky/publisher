package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import scala.collection.immutable.SortedMap
import java.io.File

object Site:
  def main(args: Array[String]): Unit =
    val site: Site = Site(
      sourceDirectoryPath = "/home/dub/Podval/dub.podval.org",
      logLevel = Level.DEBUG
    )
    site.scan()

final class Site(
  sourceDirectoryPath: String,
  logLevel: Level = Level.INFO
):
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val sourceDirectory: File =
    val result: File = File(sourceDirectoryPath).getAbsoluteFile

    Files.requireExists(result)
    Files.requireDirectory(result)

    log.info(s"source directory: $result")
    result

  private val configFile: File =
    val result: File = File(sourceDirectory, Config.fileName)
    Files.requireExists(result)
    Files.requireFile(result)
    log.info(s"configuration file: $result")
    result

  val config: Config =
    val result: Config = Config.fromYaml(Files.read(configFile))
    log.debug(s"configuration:\n"+Config.toYaml(result))
    result

  private val targetDirectory: File =
    val result: File = File(sourceDirectory, config.targetDirectoryName)
    result.mkdirs()
    Files.requireExists(result)
    Files.requireDirectory(result)
    result

  private given CanEqual[File, File] = CanEqual.derived

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
    val pageKind: PageKind = PageKind.get(sourcePath, config)

    val result: Either[String, Page] = for
      targetPath: Path <- pageKind.targetPath(sourcePath)
      page: Page = extension.flatMap(extension => Markup.all.find(_.isExtension(extension))) match
        case None =>
          Asset(
            sourcePath,
            targetPath,
            pageKind
          )
        case Some(markup) =>
          val (frontMatter: FrontMatter, content: String) = FrontMatter.read(sourcePath.file(sourceDirectory))
          MarkupPage(
            sourcePath,
            targetPath,
            pageKind,
            frontMatter,
            markup,
            markup.read(content)
          )
      _ <- pageKind.validate(page)
    yield page

    result match
      case Right(page) =>
        log.debug(page.toString)
        Some(page)
      case Left(error) =>
        log.error(error)
        None

  private var markupPages: Map[Path, MarkupPage] = Map.empty

  def scan(): Unit =
    // Enumerate all pages
    val pages: List[Page] = directoryPages(
      path = List.empty,
      directory = sourceDirectory
    )

    // Report conflicting pages
    pages
      .groupBy(_.targetPath)
      .filter(_._2.length > 1)
      .foreach((path: Path, pages: List[Page]) =>
        log.error(s"Path $path is targeted by multiple pages: ${pages.map(_.sourcePath).mkString(",")}")
      )

    // Copy asset pages
    pages
      .collect { case page: Asset => page }
      .foreach: (page: Page) =>
        Files.copy(
          fromFile = page.sourcePath.file(sourceDirectory),
          toFile = page.targetPath.file(targetDirectory)
        )
        log.debug(s"Copied: ${page.sourcePath} to ${page.targetPath}")

    // Collect markup pages
//    markupPages = SortedMap(pages
//      .collect { case page: MarkupPage => page }
//      .map(page => page.path -> page)
//    *)
