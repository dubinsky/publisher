package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.{Schema, SchemaError}
import zio.blocks.schema.yaml.{Yaml, YamlCodec, YamlFormat, YamlReader, YamlWriter}
import zio.blocks.typeid.TypeId
import scala.util.control.NonFatal

final case class FrontMatter(
  title: Option[String] = None,
  description: Option[String] = None,
  author: Option[String] = None,
  lang: Option[String] = None,
  math: Boolean = false,
  tags: List[String] = List.empty,
  categories: List[String] = List.empty,
  aliases: List[String] = List.empty,
  permalink: Option[String] = None,
  date: Option[Date] = None,
  icon: Option[FontAwesome.Icon] = None,
  modified_time: Option[Date] = None, // TODO kebab breaks this!
  headerPage: Option[FrontMatter.HeaderPage] = None
):
  def merge(default: FrontMatter): FrontMatter = copy(
    title = title.orElse(default.title),
    description = description.orElse(default.description),
    lang = lang.orElse(default.lang),
    permalink = permalink.orElse(default.permalink),
    icon = icon.orElse(default.icon),
    headerPage = (headerPage, default.headerPage) match
      case (None, None) => None
      case (None, Some(headerPageDefault)) => Some(headerPageDefault)
      case (Some(headerPage), None) => Some(headerPage)
      case (Some(headerPage), Some(headerPageDefault)) => Some(headerPage.merge(headerPageDefault))
  )

  // TODO this with not survive round trip once FrontMatter becomes a case class and `.copy()` is used!
  private var extraKeys: Chunk[(Yaml, Yaml)] = Chunk.empty

  private var absent: Boolean = false

  def write: String = if absent then "" else
    val mapping: String = YamlWriter.write(Yaml.Mapping(
      FrontMatter.codec.encodeValue(this).asInstanceOf[Yaml.Mapping].entries ++ extraKeys
    ))

    s"---\n$mapping\n---\n"

object FrontMatter:
  // TODO adjust schema/codec so that the (lower-case) name of the variant works, without requiring
  //icon:
  //  name: folder
  //  style:
  //    Regular: {}
//  def main(args: Array[String]): Unit = println(FrontMatter(icon = Some(FontAwesome.folder)).write)

  final case class HeaderPage(
    include: Boolean = false,
    priority: Option[Int] = None
  ):
    def merge(default: HeaderPage): HeaderPage = copy(
      priority = priority.orElse(default.priority)
    )

  val empty: FrontMatter = FrontMatter()

  val absent: FrontMatter =
    val result = FrontMatter()
    result.absent = true
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
      (Left(PageError.Parsing(sourcePath, s"Malformed FrontMatter: [$input]", Some(yamlError))), content)
      
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
