package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.Schema
import zio.blocks.schema.xml.{XmlBinaryCodec, XmlBinaryCodecDeriver}
import zio.blocks.schema.yaml.{YamlBinaryCodec, YamlBinaryCodecDeriver}
import java.io.File

final case class Config(
  targetDirectoryName: String = "_site",
  blogDirectoryName: String = BlogPost.sourceDirectoryName,
  // TODO get out of `.obsidian/daily-notes.json/folder`
  dailyNotesDirectoryName: String = DailyNote.sourceDirectoryName,
  title: String,
  author: String,
  email: String,
  description: String,
  exclude: List[String]
):
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  private val includedSet: Set[String] = Set(
    blogDirectoryName,
    dailyNotesDirectoryName
  )
  
  private val excludeSet: Set[String] = exclude.toSet

  def exclude(file: File): Boolean =
    val name: String = file.getName
    if includedSet.contains(name) then false
    else if excludeSet.contains(name) then
      log.debug(s"Excluded: $name")
      true
    else
      Config.special.contains(name) ||
      Config.specialStartsWith.exists(name.startsWith)

object Config:
  val fileName: String = "_config.yml"

  private val special: Set[String] = Set(
    "node_modules",
    "vendor",
    ".jekyll-cache",
    ".sass-cache",
    "Gemfile",
    "Gemfile.lock",
    "LICENSE",
    "README.md"
  )

  private val specialStartsWith: Set[String] = Set(
    ".",
    "_", // TODO _drafts can be included explicitly
    "~",
    "#"
  )

  given Schema[Config] = Schema.derived

  val yamlCodec: YamlBinaryCodec[Config] = Schema[Config].derive(YamlBinaryCodecDeriver)

  def fromYaml(string: String): Config = yamlCodec.decode(string) match
    case Left(schemaError) => throw IllegalArgumentException("Failed to decode config", schemaError)
    case Right(config) => config

  def toYaml(config: Config): String = yamlCodec.encodeToString(config)

  val xmlCodec: XmlBinaryCodec[Config] = Schema[Config].derive(XmlBinaryCodecDeriver)

  def fromXml(string: String): Config = xmlCodec.decode(string) match
    case Left(schemaError) => throw IllegalArgumentException("Failed to decode config", schemaError)
    case Right(config) => config

  def toXml(config: Config): String = xmlCodec.encodeToString(config)
