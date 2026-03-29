package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File

object Site:
  def main(args: Array[String]): Unit =
    val site: Site = Site(
      sourceDirectoryPath = "/home/dub/Podval/dub.podval.org",
      treatWarningsAsErrors = true,
      logLevel = Level.DEBUG
    )
    site.generate match
      case Right(()) => println("Done!")
      case Left(error) => println(s"Error generating site: $error")

final class Site(
  sourceDirectoryPath: String,
  treatWarningsAsErrors: Boolean,
  logLevel: Level = Level.INFO
):
  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)

  private val warnings: Warnings = Warnings(treatWarningsAsErrors = true)

  private val links: Links = Links(warnings)

  private given CanEqual[File, File] = CanEqual.derived

  def generate: Either[PageError, Unit] =
    for
      // Enumerate all pages
      pagesRaw: List[Page] <- directoryPages(List.empty, config.sourceDirectory)

      // Report conflicting pages
      pages: List[Page] <- warnings.recover(deDup(pagesRaw))(pagesRaw)

      // Resolve links
      // TODO do the warning.recover dance
      _ = pages.collect { case page: Page.MarkupPage => page }.foreach(_.resolveLinks(links))

      // TODO Add backlinks

      // TODO generate tags, maps etc.

      // TODO generate reports

      // Write pages
      _ = pages.foreach(writePage)

      // TODO Render markup pages
    yield ()

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

    val result: Either[PageError, Option[Page]] = Page.makePage(
      sourcePath = sourcePath,
      sourceFile = sourcePath.file(config.sourceDirectory),
      pageKind = PageKind.special
        .find(special => sourcePath.startsWith(config.specialPageKindSourcePathStartsWith(special)))
        .getOrElse(PageKind.Plain),
      warnings = warnings
    )
    
    result match
      case Right(Some(page)) => log.debug(s"Read: $page")
      case _ =>
    
    result
  
  private def deDup(pages: List[Page]): Either[PageError, List[Page]] =
    val duplicate: Map[Path, List[Page]] = pages.groupBy(_.targetPath).filter(_._2.length > 1)
    if duplicate.isEmpty
    then Right(pages)
    else Left(PageError(duplicate.map((path: Path, pages: List[Page]) =>
      s"Path $path is targeted by multiple pages: ${pages.map(_.sourcePath).mkString(",")}"
    ).mkString("\n")))

  private def writePage(page: Page): Unit =
    val targetFile: File = page.targetPath.file(config.targetDirectory)
    page match
      case asset: Page.Asset =>
        Files.copy(fromFile = page.sourcePath.file(config.sourceDirectory), toFile = targetFile)
        
      case syntheticAsset: Page.SyntheticAsset =>
        Files.write(syntheticAsset.content, targetFile)
        
      case markupPage: Page.MarkupPage =>
        // TODO wrap markupPage.content into layouts
        // TODO render XML to String
        // TODO Files.write(markupPage.content, targetFile)
        ()

    log.debug(s"Wrote: $page")

  