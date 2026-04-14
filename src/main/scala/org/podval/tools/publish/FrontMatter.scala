package org.podval.tools.publish

import zio.blocks.schema.SchemaError
import zio.blocks.schema.yaml.Yaml
import java.time.LocalDate

final class FrontMatter(
  val isAbsent: Boolean,
  keys: Map[String, Yaml],
  val date: Option[LocalDate] = None
) extends YamlMapping(keys):
  def write: String =
    if isAbsent then ""
    else "---\n" ++ writeYamlMapping ++ "\n---\n"

  def title: Option[String] = toString("title")
  def layout: Option[String] = toString("layout")
  def tags: List[String] = toStrings("tags")
  def categories: List[String] = toStrings("categories")

object FrontMatter:
  val absent: FrontMatter = FrontMatter(isAbsent = true, keys = Map.empty)
  val empty: FrontMatter = FrontMatter(isAbsent = false, keys = Map.empty)

  def parse(input: String): (Either[SchemaError, FrontMatter], String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (Right(absent), input) else
      val frontMatterInput: String = input.substring(3, frontMatterEnd)
      val frontMatterLines: Int = frontMatterInput.count(_ == '\n') + 2
      val frontMatter: Either[SchemaError, FrontMatter] =
        if frontMatterInput.isEmpty then Right(empty) else 
          for
            mapping: Map[String, Yaml] <- YamlMapping.parse(frontMatterInput)
            date <- YamlMapping.toDate(YamlMapping.toString(mapping.get("date")))
          yield
            FrontMatter(isAbsent = false, keys = mapping, date = date)
      val content: String =
        "\n"*frontMatterLines +
        input.substring(frontMatterEnd + 5)
      (frontMatter, content)
