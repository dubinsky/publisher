package org.podval.tools.publish

import zio.blocks.schema.Schema
import zio.blocks.schema.xml.{XmlBinaryCodec, XmlBinaryCodecDeriver}
import zio.blocks.schema.yaml.{YamlBinaryCodec, YamlBinaryCodecDeriver}

final case class Config(
  title: String,
  author: String,
  email: String,
  description: String,
  exclude: List[String]
)

object Config:
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
