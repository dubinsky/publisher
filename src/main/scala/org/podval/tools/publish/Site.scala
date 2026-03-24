package org.podval.tools.publish

import java.io.File

// TODO Path as a class, not type alias; Path.Directory, Path.Page (with extension)
// TODO Set[Page]
// TODO: tags.html, sitemap.xml, robots.txt, feed.xml
// TODO auto-generation with filesystem watch
// TODO logging instead of println
// TODO log ignored directories
// TODO push into a bucket
// TODO checks:
// - links with http[s]://
// - broken links
// - inconsistent titles
object Site:
  private val configFileName: String = "publisher-config.yaml"
  private val destinationDirectoryName = "__site"

  def main(args: Array[String]): Unit =
    val site: Site = Site(
      rootDirectoryPath = "/home/dub/Podval/dub.podval.org"
    )
    site.report()
    site.pages.foreach(println)
    site.copyCopiedPages()

final class Site(rootDirectoryPath: String):
  private val rootDirectory: File =
    val result: File = File(rootDirectoryPath).getAbsoluteFile

    Files.requireExists(result)
    Files.requireDirectory(result)

    result

  private val destinationDirectory: File =
    val result: File = File(rootDirectory, Site.destinationDirectoryName)

    if !result.exists then result.mkdirs()

    Files.requireExists(result)
    Files.requireDirectory(result)

    result

  private val configFile: File =
    val result: File = File(rootDirectory, Site.configFileName)

    Files.requireExists(result)
    Files.requireFile(result)

    result

  val config: Config = Config.fromYaml(Files.read(configFile))

  def report(): Unit =
    println(s"root directory: $rootDirectory")
    println(s"destination directory: $destinationDirectory")
    println(s"configuration file: $configFile")
    println(s"configuration")
    println(s"---")
    //    println(Config.toXml(config))
    println(Config.toYaml(config))
    println(s"---")

  private given CanEqual[File, File] = CanEqual.derived

  private def excluded(file: File): Boolean =
    config.exclude.contains(file.getName) ||
      file == destinationDirectory ||
      file == configFile

  lazy val pages: List[Page] = pages(
    List.empty,
    Files.list(rootDirectory).filterNot(excluded)
  )

  private def pages(path: Path, files: List[File]): List[Page] = files
    .map(_.getName)
    .map(path :+ _)
    .flatMap(pages)

  private def pages(path: Path): List[Page] =
    val file: File = Files.file(rootDirectory, path)
    if !file.isFile
    then pages(path, Files.list(file))
    else List:
      // TODO detect isIndex pages
      val (name: String, extension: Option[String]) = Files.nameAndExtension(file.getName)
      if extension.contains("md")
      then MarkdownPage(path, fromFile = file)
      else Page.Copied(path, fromFile = file)

  def copyCopiedPages(): Unit = pages
    .collect { case copied: Page.Copied => copied }
    .foreach(copied => copied.copyTo(Files.file(destinationDirectory, copied.path)))
