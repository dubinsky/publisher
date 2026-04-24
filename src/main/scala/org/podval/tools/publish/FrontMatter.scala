package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.{Schema, SchemaError}
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader, YamlWriter}
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

final class FrontMatter(
  val layout: Option[String] = None,
  val title: Option[String] = None,
  val description: Option[String] = None,
  val permalink: Option[String] = None,
  val author: Option[String] = None,
  val lang: Option[String] = None,
  val math: Boolean = false,
  val tags: List[String] = List.empty,
  val categories: List[String] = List.empty,
  val date: Option[Date] = None
):
  private var extraKeys: Chunk[(Yaml, Yaml)] = Chunk.empty

  private var absent: Boolean = false
  def setAbsent(): Unit = absent = true
  def isAbsent: Boolean = absent

  def write: String = if isAbsent then "" else
    val mapping: String = YamlWriter.write(Yaml.Mapping(
      FrontMatter.codec.encodeValue(this).asInstanceOf[Yaml.Mapping].entries ++ extraKeys
    ))

    s"---\n$mapping\n---\n"

object FrontMatter:
  val empty: FrontMatter = FrontMatter()

  val absent: FrontMatter =
    val result = FrontMatter()
    result.setAbsent()
    result

  private val schema: Schema[FrontMatter] = Schema.derived

  // TODO these are from the schema; since codec turns the names to kebab case, some are not going to match...
  private val fieldNames: Set[String] = schema
    .reflect
    .asRecord
    .get
    .fields
    .map(_.name)
    .toSet

  private val codec: YamlCodec[FrontMatter] = schema
    .deriving(YamlFormat.deriver)
    .instance(TypeId.of[Date], Date.codec)
    .derive

  def parse(sourcePath: Path, input: String): (Either[PageError, FrontMatter], String) = parse(input) match
    case (Right(frontMatter), content) =>
      (Right(frontMatter), content)
    case (Left(yamlError), content) =>
      (Left(PageError.Parsing(sourcePath, "Malformed FrontMatter", Some(yamlError))), content)
      
  def parse(input: String): (Either[SchemaError, FrontMatter], String) =
    val frontMatterEnd: Int = if !input.startsWith("---\n") then -1 else input.indexOf("\n---\n", 3)
    if frontMatterEnd == -1 then (Right(absent), input) else
      val frontMatterInput: String = input.substring(3, frontMatterEnd)
      val frontMatter: Either[SchemaError, FrontMatter] =
        if frontMatterInput.isEmpty
        then Right(empty)
        else decode(frontMatterInput)
      val frontMatterLines: Int = frontMatterInput.count(_ == '\n') + 2
      val content: String =
        "\n"*frontMatterLines +
        input.substring(frontMatterEnd + 5)
      (frontMatter, content)


  private def decode(input: String): Either[SchemaError, FrontMatter] =
    try
      val yaml: Yaml = YamlReader.read(input)
      val result: FrontMatter = codec.decodeValue(yaml)
      result.extraKeys = yaml
        .asInstanceOf[Yaml.Mapping]
        .entries
        .filter(_._1 match
          case Yaml.Scalar(key, _) => !fieldNames.contains(key)
          case _ => true
        )
      Right(result)
    catch
      case error: Throwable if NonFatal(error) => new Left(SchemaError(error.getMessage))

//  private def stringsCodec: YamlCodec[List[String]] = new YamlCodec[List[String]]:
//    override def decodeValue(value: Yaml): List[String] = value match
//      case Yaml.Sequence(elements) => elements
//        .collect { case Yaml.Scalar(value, _) => value }
//        .toList
//      case Yaml.Scalar(value, _) =>
//        List(value)
//      case _ =>
//        List.empty
//
//
//    override def encodeValue(strings: List[String]): Yaml = ???
//