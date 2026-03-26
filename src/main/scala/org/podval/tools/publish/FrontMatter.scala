package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.yaml.{Yaml, YamlError, YamlReader, YamlWriter}
import java.time.LocalDate

sealed trait FrontMatter:
  def isAbsent: Boolean
  def get(key: String): Option[Yaml]
  def write: String

  final def title: Option[String] = toString("title")
  final def layout: Option[String] = toString("layout")
  final def tags: List[String] = toStrings("tags")
  final def categories: List[String] = toStrings("categories")
  final def date: Option[LocalDate] = toDate("date")

  final def toString(key: String): Option[String] = get(key) match
    case Some(Yaml.Scalar(value, _)) => Some(value)
    case _ => None

  final def toStrings(key: String): List[String] = get(key) match
    case Some(Yaml.Sequence(elements)) => elements
      .collect { case Yaml.Scalar(value, _) => value }
      .toList
    case Some(Yaml.Scalar(value, _)) =>
      List(value)
    case _ =>
      List.empty

  final def toDate(key: String): Option[LocalDate] =
    for
      string: String <- toString(key)
      result <- Util.parseDate(string) match
        case Right(date) => Some(date)
        case Left(error) =>
          // TODO log s"Malformed date: $string"
          None
    yield result
  

object FrontMatter:
  object Absent extends FrontMatter:
    override def isAbsent: Boolean = true
    override def get(key: String): Option[Yaml] = None
    override def write: String = ""

  object Empty extends FrontMatter:
    override def isAbsent: Boolean = false
    override def get(key: String): Option[Yaml] = None
    override def write: String = "---\n---\n"

  private final class Regular(keys: Map[String, Yaml]) extends FrontMatter:
    def isAbsent: Boolean = false
    def get(key: String): Option[Yaml] = keys.get(key)
    def write: String =
      "---\n" ++
      YamlWriter.write(Yaml.Mapping.fromStringKeys(keys.toList *)) ++
      "\n---\n"

  def parse(input: String): (Either[YamlError, FrontMatter], String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (Right(Absent), input) else
      val frontMatterInput: String = input.substring(3, frontMatterEnd)
      val frontMatterLines: Int = frontMatterInput.count(_ == '\n') + 2
      val content: String =
        "\n"*frontMatterLines +
        input.substring(frontMatterEnd + 5)
      if frontMatterInput.isEmpty then (Right(Empty), content) else
        val result: Either[YamlError, Regular] = for
          yaml: Yaml <- YamlReader.read(frontMatterInput)
          mapping: List[(Yaml, Yaml)] <- yaml match
            case Yaml.Mapping(entries: Chunk[(Yaml, Yaml)]) => Right(entries.toList)
            case _ => Left(YamlError("FrontMatter must be a mapping", 1, 1))
          stringMapping: Seq[(String, Yaml)] <- Util.sequence(mapping)((keyYaml, value) => keyYaml match
            case Yaml.Scalar(key, _) => Right((key, value))
            case _ => Left(YamlError(s"FrontMatter key must be a string: $keyYaml", 1, 1))
          )
        yield Regular(stringMapping.toMap)

        (result, content)

