package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.SchemaError
import zio.blocks.schema.yaml.{Yaml, YamlReader, YamlWriter}
import java.time.{LocalDate, LocalDateTime, OffsetDateTime}
import java.time.format.DateTimeParseException

// Note: ZIO Blocks YAML parse/write support for the markup front-matter and configuration file.
// Attempts to use ZIO Blocks YAML decode/encode support failed:
// - I want to round-trip keys not covered by the object being decoded, and there is no easy way to do it;
// - I want to use key names known from Jekyll configuration file, but ZIO Blocks hard-codes kebab case for field names;
// - I want to accept dates that are in *either* of the short, full, zoned formats, not in exactly one of them.
abstract class YamlMapping(keys: Map[String, Yaml]):
  final def get(key: String): Option[Yaml] = keys.get(key)

  final def isEmpty: Boolean = keys.isEmpty

  final def writeYamlMapping: String = YamlWriter.write(Yaml.Mapping.fromStringKeys(keys.toList *))

  final def toString(key: String): Option[String] = YamlMapping.toString(get(key))

  final def toString(key: String, default: String): String = toString(key).getOrElse(default)
  
  final def toStringDo(key: String): String = toString(key).getOrElse(throw IllegalArgumentException(s"No value for $key"))

  final def toStrings(key: String): List[String] = YamlMapping.toStrings(get(key))

object YamlMapping:
  def parse(input: String): Either[SchemaError, Map[String, Yaml]] =
    for
      yaml: Yaml = YamlReader.read(input)
      mapping: Chunk[(Yaml, Yaml)] <- yaml match
        case Yaml.Mapping(entries: Chunk[(Yaml, Yaml)]) => Right(entries)
        case _ => Left(SchemaError("Must be a Yaml.Mapping"))
      stringMapping: List[(String, Yaml)] <- Util.sequence(mapping.toList)((keyYaml, value) => keyYaml match
        case Yaml.Scalar(key, _) => Right((key, value))
        case _ => Left(SchemaError(s"Must be a string: $keyYaml"))
      )
    yield
      stringMapping.toMap

  def toString(value: Option[Yaml]): Option[String] = value match
    case Some(Yaml.Scalar(value, _)) => Some(value)
    case _ => None

  def toStrings(value: Option[Yaml]): List[String] = value match
    case Some(Yaml.Sequence(elements)) => elements
      .collect { case Yaml.Scalar(value, _) => value }
      .toList
    case Some(Yaml.Scalar(value, _)) =>
      List(value)
    case _ =>
      List.empty

  def toDate(value: Option[String]): Either[SchemaError, Option[LocalDate]] = value match
    case None => Right(None)
    case Some(value) =>
      try
        // 2026-03-29
        Right(Some(LocalDate.parse(value)))
      catch case e: DateTimeParseException =>
        try
          // 2010-01-28T14:24:00
          Right(Some(LocalDateTime.parse(value).toLocalDate))
        catch case e: DateTimeParseException =>
          try
            //2010-01-28T14:24:00.004-05:00
            Right(Some(OffsetDateTime.parse(value).toLocalDate))
          catch case e: DateTimeParseException =>
            Left(SchemaError(s"Malformed date: $value $e"))

