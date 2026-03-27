package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
//import scala.collection.immutable.SortedMap
import java.io.File
import Util.ifDefined

object Site:
  def main(args: Array[String]): Unit =
    val site: Site = Site(
      sourceDirectoryPath = "/home/dub/Podval/dub.podval.org",
      logLevel = Level.DEBUG
    )
    site.generate()

final class Site(
  sourceDirectoryPath: String,
  logLevel: Level = Level.INFO
):
  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)

  private val warnings: Warnings = Warnings(treatWarningsAsErrors = true)

  private given CanEqual[File, File] = CanEqual.derived

  def generate(): Unit =
    val result: Either[PageError, Unit] =
      for
        // Enumerate all pages
        pagesRaw: List[Page] <- directoryPages(List.empty, config.sourceDirectory)

        // Report conflicting pages
        pages: List[Page] <- warnings.recover(deDup(pagesRaw))(pagesRaw)
        
        links: Links = Links(pages)
        _ = links.resolveLinks()
        
        // TODO Add backlinks

        // Copy assets
        _ = copyAssets(pages)

        // TODO Render markup pages
        
        // TODO generate tags, maps etc.
        
        // TODO generate reports
      yield ()

    result.swap.toOption.foreach(error => log.error("Error generating site", error))
  
  // Collect markup pages - TODO all!
  //    markupPages = SortedMap(pages
  //      .collect { case page: MarkupPage => page }
  //      .map(page => page.path -> page)
  //    *)
  
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
    val markup: Option[Markup] = extension.flatMap(extension => Markup.all.find(_.isExtension(extension)))
    val pageKind: PageKind = PageKind.get(sourcePath, config)

    ifDefined(warnings.recoverNone(pageKind.targetPath(sourcePath))): (targetPath: Path) =>
      ifDefined(markup match
        case None => Right(Some(Asset(
          sourcePath = sourcePath,
          targetPath = targetPath, 
          pageKind = pageKind
        )))
        case Some(markup) => MarkupPage(
          markup = markup, 
          sourceFile = sourcePath.file(config.sourceDirectory), 
          sourcePath = sourcePath,
          targetPath = targetPath,
          pageKind = pageKind, 
          warnings = warnings
        )
      ): (page: Page) =>
        ifDefined(warnings.recoverNone(pageKind.validate(page).map(_ => page))): (page: Page) =>
          log.debug(page.toString)
          Right(Some(page))

  private def deDup(pages: List[Page]): Either[PageError, List[Page]] =
    val duplicate: Map[Path, List[Page]] = pages.groupBy(_.targetPath).filter(_._2.length > 1)
    if duplicate.isEmpty
    then Right(pages)
    else Left(PageError(duplicate.map((path: Path, pages: List[Page]) =>
      s"Path $path is targeted by multiple pages: ${pages.map(_.sourcePath).mkString(",")}"
    ).mkString("\n")))

  private def copyAssets(pages: List[Page]): Unit = pages
    .collect { case page: Asset => page }
    .foreach: (page: Page) =>
      Files.copy(
        fromFile = page.sourcePath.file(config.sourceDirectory),
        toFile = page.targetPath.file(config.targetDirectory)
      )
      log.debug(s"Copied: ${page.sourcePath} to ${page.targetPath}")
