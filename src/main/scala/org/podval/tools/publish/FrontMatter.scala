package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.chunk.Chunk
import zio.blocks.schema.yaml.{Yaml, YamlError, YamlReader, YamlWriter}
import java.io.File
import scala.annotation.tailrec

final class FrontMatter(
  keys: Map[String, Yaml],
  val sourceLines: Int
):
  require(sourceLines != 1)

  def isNone: Boolean = sourceLines == 0

  def get(key: String): Option[Yaml] = keys.get(key)

  def write: String = if isNone then "" else
    "---\n" ++
    YamlWriter.write(Yaml.Mapping.fromStringKeys(keys.toList *)) ++
    "\n---\n"

object FrontMatter:
  val none: FrontMatter = FrontMatter(keys = Map.empty, sourceLines = 0)
  val empty: FrontMatter = FrontMatter(keys = Map.empty, sourceLines = 2)

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def parse(input: String): (Either[YamlError, FrontMatter], String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (Right(none), input) else
      val frontMatterInput: String = input.substring(3, frontMatterEnd)
      val frontMatterLines: Int = frontMatterInput.count(_ == '\n') + 2
      val content: String = input.substring(frontMatterEnd + 5)
      if frontMatterInput.isEmpty then (Right(empty), content) else
        val result = for
          yaml: Yaml <- YamlReader.read(frontMatterInput)
          mapping: List[(Yaml, Yaml)] <- yaml match
            case Yaml.Mapping(entries: Chunk[(Yaml, Yaml)]) => Right(entries.toList)
            case _ => Left(YamlError("FrontMatter must be a mapping", 1, 1))
          stringMapping: Seq[(String, Yaml)] <- sequence(mapping)((keyYaml, value) => keyYaml match
            case Yaml.Scalar(key, _) => Right((key, value))
            case _ => Left(YamlError(s"FrontMatter key must be a string: $keyYaml", 1, 1))
          )
        yield FrontMatter(stringMapping.toMap, frontMatterLines)
        (result, content)

  def read(file: File): (FrontMatter, String) = FrontMatter
    .parse(Files.read(file)) match
    case (Right(result), content) => (result, content)
    case (Left(yamlError), content) =>
      log.error(s"FrontMatter error in $file", yamlError)
      (FrontMatter.none, content)

  private def sequence[A, E, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    @tailrec
    def loop(as: List[A], result: List[B]): Either[E, List[B]] = as match
      case Nil => Right(result)
      case a :: as => f(a) match
        case Right(b) => loop(as, result :+ b)
        case Left(e) => Left(e)

    loop(as, List.empty)

