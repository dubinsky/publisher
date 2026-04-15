package org.podval.tools.publish

import org.podval.tools.publish.html.Html
import org.slf4j.{Logger, LoggerFactory}
import org.slf4j.event.Level
import java.io.File

object Site:
  def main(args: Array[String]): Unit =
    val site: Site = Site(
      sourceDirectoryPath = "/home/dub/Podval/dub.podval.org",
      treatWarningsAsErrors = true,
      reformatSourceFiles = false,
      logLevel = Level.DEBUG
    )
    site.generate match
      case Right(()) => println("Done!")
      case Left(error) => println(s"Error generating site: $error")

final class Site(
  sourceDirectoryPath: String,
  treatWarningsAsErrors: Boolean,
  reformatSourceFiles: Boolean,
  logLevel: Level = Level.INFO
):
  Logging.configureLogBack(level = logLevel, useLogStash = false)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val config: Config = Config(sourceDirectoryPath)

  private val warnings: Warnings = Warnings(treatWarningsAsErrors = true)

  private given CanEqual[File, File] = CanEqual.derived

  def generate: Either[PageError, Unit] = for
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
    // TODO do the warning.recover/sequence dance
    _ = pages.collect { case page: Page.MarkupPage => page }.foreach(_.resolveLinks(links))

    // Write site
    _ <- Util.sequence(pages)(writePage(_, links))
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

    val result: Either[PageError, Option[Page]] = Page.makePage(
      sourcePath = sourcePath,
      sourceFile = sourcePath.file(config.sourceDirectory),
      reformatSourceFile = reformatSourceFiles,
      warnings = warnings,
      pageKind = PageKind(sourcePath, config)
    )

    result match
      case Right(Some(page)) => log.debug(s"Read: $page")
      case _ =>

    result

  private def writePage(page: Page, links: Links): Either[PageError, Unit] =
    val targetFile: File = page.targetPath.file(config.targetDirectory)
    val result: Either[PageError, Unit] = page match
      case asset: Page.Asset =>
        Right(Files.copy(toFile = targetFile, fromFile = page.sourcePath.file(config.sourceDirectory)))

      case syntheticAsset: Page.SyntheticAsset =>
        Right(Files.write(file = targetFile, content = syntheticAsset.content))

      case markupPage: Page.MarkupPage =>
        val layout: Layout = Layout.Default // TODO calculate based on FrontMatter and PageKind
        for xml <- markupPage.render yield
          val content = layout.render(xml, links.backLinks(markupPage))
          Files.write(file = targetFile, content = Html.write(content))
          ()

    result.foreach(_ => log.debug(s"Wrote: $page"))
    result


