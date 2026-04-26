package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.Schema
import zio.blocks.schema.yaml.{YamlCodec, YamlFormat}
import java.io.File

final class Config(
  val title: String,
  val description: String,
  val author: String,
  val email: String,

  val timezone: Option[String] = None,
  val lang: Option[String] = None,

  val exclude: List[String] = List.empty,
  val headerPages: List[String], // TODO freaking kebab case!

  val target: Config.Target = Config.Target(),
  val blog: Config.Blog = Config.Blog(),

  val analytics: Config.Analytics = Config.Analytics(),
  val social: Config.Social = Config.Social()
):
  private var sourceDirectoryOpt: Option[File] = None
  def setSourceDirectory(value: File): Unit = sourceDirectoryOpt = Some(value)
  def sourceDirectory: File = sourceDirectoryOpt.get

  lazy val targetDirectory: File =
    val result: File = File(sourceDirectory, target.directory)
    result.mkdirs()
    Files.requireExists(result)
    Files.requireDirectory(result)
    result

  def blogDirectoryName: String = blog.source.getOrElse(Locator.BlogPost.sourceDirectoryNameDefault)
  def dailyNotesDirectoryName: Option[String] = blog.daily // TODO get out of the Obsidian configuration
  
  private val includedSet: Set[String] = 
    Set(blogDirectoryName) ++
    dailyNotesDirectoryName.toSet

  private val excludeSet: Set[String] = exclude.toSet

  def exclude(file: File): Boolean =
    val name: String = file.getName
    if includedSet.contains(name) then false
    else if excludeSet.contains(name) then
      Config.log.debug(s"excluded: $name")
      true
    else
      Config.special.contains(name) ||
      Config.specialStartsWith.exists(name.startsWith)

object Config:
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  // TODO also exclude stuff from `.gitignore` if present

  private val special: Set[String] = Set(
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md",
    "build",
    "build.gradle",
    //    "bundle",
    "gradle",
    "gradlew",
    "gradlew.bat",
    "node_modules",
    "settings.gradle",
    "src",
    "tags.html", // my Jekyll customization; I am going to synthesize it
    "vendor",
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_",
    "~",
    "#"
  )

  final class Target(
    val directory: String = "_site",
    val url: Option[String] = None,
    val base: Option[String] = None // TODO the subpath of your site, e.g. /blog
  )

  final class Blog(
    val source: Option[String] = None,
    val target: Option[String] = None, // TODO implement
    val daily: Option[String] = None // TODO write those to the blog target
  )

  final class Analytics(
    val google: Option[String] = None
  )

  final class Social(
    val github: Option[String] = None,
    val twitter: Option[String] = None,
    val linkedin: Option[String] = None
  )

  private val schema: Schema[Config] = Schema.derived

  private val codec: YamlCodec[Config] = schema
    .deriving(YamlFormat.deriver)
    .derive

  val fileName: String = "_site_config.yml"

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

    codec.decode(Files.read(configFile)) match
      case Left(error) => throw IllegalArgumentException("Malformed Config", error)
      case Right(result) =>
        result.setSourceDirectory(sourceDirectory)
        log.debug(s"Config:\n" + codec.encodeToString(result))
        result
