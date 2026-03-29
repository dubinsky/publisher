package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.yaml.Yaml
import java.io.File

final class Config(
  val sourceDirectory: File,
  keys: Map[String, Yaml]
) extends YamlMapping(keys):
  def targetDirectoryName: String = toString("targetDirectoryName", "_site")
  def blogDirectoryName: String = toString("blogDirectoryName", PageKind.BlogPost.sourceDirectoryName)
  def dailyNotesDirectoryName: String = toString("dailyNotesDirectoryName", PageKind.DailyNote.sourceDirectoryName)
  def title: String = toStringDo("title")
  def author: String = toStringDo("author")
  def email: String = toStringDo("email")
  def description: String = toStringDo("description")
  def exclude: List[String] = toStrings("exclude")

  lazy val targetDirectory: File =
    val result: File = File(sourceDirectory, targetDirectoryName)
    result.mkdirs()
    Files.requireExists(result)
    Files.requireDirectory(result)
    result
    
  private val includedSet: Set[String] = Set(
    blogDirectoryName,
    dailyNotesDirectoryName
  )

  private val excludeSet: Set[String] = exclude.toSet

  def exclude(file: File): Boolean =
    val name: String = file.getName
    if includedSet.contains(name) then false
    else if excludeSet.contains(name) then
      Config.log.debug(s"Excluded: $name")
      true
    else
      Config.special.contains(name) ||
      Config.specialStartsWith.exists(name.startsWith)

  def specialPageKindSourcePathStartsWith(pageKind: PageKind.Special): String = pageKind match
    case PageKind.BlogPost => blogDirectoryName
    case PageKind.DailyNote => dailyNotesDirectoryName

object Config:
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  val fileName: String = "_config.yml"

  // TODO also exclude stuff from `.gitignore` if present

  private val special: Set[String] = Set(
    "node_modules",
    "vendor",
//    "bundle",
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md",
    "build.gradle",
    "gradle",
    "gradlew",
    "gradlew.bat",
    "settings.gradle",
    "src"
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_",
    "~",
    "#"
  )
  
  def apply(sourceDirectoryPath: String): Config =
    val sourceDirectory: File =
      val result: File = File(sourceDirectoryPath).getAbsoluteFile

      Files.requireExists(result)
      Files.requireDirectory(result)

      log.info(s"source directory: $result")
      result

    val configFile: File =
      val result: File = File(sourceDirectory, Config.fileName)
      Files.requireExists(result)
      Files.requireFile(result)
      log.info(s"configuration file: $result")
      result

    YamlMapping.parse(Files.read(configFile)) match
      case Left(error) => throw IllegalArgumentException("Malformed Config", error)
      case Right(mapping) =>
        val result = new Config(sourceDirectory, mapping)
        log.debug(s"Config:\n" + result.writeYamlMapping)
        result

