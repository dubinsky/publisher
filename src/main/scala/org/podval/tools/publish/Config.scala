package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.Schema
import zio.blocks.schema.xml.{XmlBinaryCodec, XmlBinaryCodecDeriver}
import zio.blocks.schema.yaml.{YamlBinaryCodec, YamlBinaryCodecDeriver}
import java.io.File

final case class Config(
  treatWarningsAsErrors: Boolean = true,
  targetDirectoryName: String = "_site",
  blogDirectoryName: String = BlogPost.sourceDirectoryName,
  dailyNotesDirectoryName: String = DailyNote.sourceDirectoryName,
  title: String,
  author: String,
  email: String,
  description: String,
  exclude: List[String]
):
  private var sourceDirectoryOpt: Option[File] = None
  private def setSourceDirectory(value: File): Unit = sourceDirectoryOpt = Some(value)
  def sourceDirectory: File = sourceDirectoryOpt.get
  
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

  given Schema[Config] = Schema.derived

  private val yamlCodec: YamlBinaryCodec[Config] = Schema[Config].derive(YamlBinaryCodecDeriver)

  private def fromYaml(string: String): Config = yamlCodec.decode(string) match
    case Left(schemaError) => throw IllegalArgumentException("Failed to decode config", schemaError)
    case Right(config) => config

  private def toYaml(config: Config): String = yamlCodec.encodeToString(config)

  private val xmlCodec: XmlBinaryCodec[Config] = Schema[Config].derive(XmlBinaryCodecDeriver)

  def fromXml(string: String): Config = xmlCodec.decode(string) match
    case Left(schemaError) => throw IllegalArgumentException("Failed to decode config", schemaError)
    case Right(config) => config

  def toXml(config: Config): String = xmlCodec.encodeToString(config)

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

    val result: Config = Config.fromYaml(Files.read(configFile))
    log.debug(s"configuration:\n" + Config.toYaml(result))
    result.setSourceDirectory(sourceDirectory)
    result
