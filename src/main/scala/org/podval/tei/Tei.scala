package org.podval.tei

import org.podval.xml.RawXml
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlCodec, XmlFormat}
import zio.blocks.typeid.TypeId

// TODO @xmlAttribute("id") and @xmlNamespace() annotations mentioned in the documentation
// do not work! Raw @Modifier.config("xml.attribute", "") does...
// File te issue.

@Modifier.config("xml.namespace.uri", "http://www.tei-c.org/ns/1.0")
@Modifier.config("xml.namespace.prefix", "tei")
final class TEI(
  val teiHeader: TeiHeader,
  val text: Text
)

final class TeiHeader(
)

final class Text(
  @Modifier.config("xml.attribute", "") val lang: Option[String] = None,
  val front: Option[Front] = None,
  val body: RawXml
)

final class Front(
)

object Tei:
  val mimeType = "application/tei+xml"

  given schema: Schema[TEI] = Schema.derived

  private val codec: XmlCodec[TEI] = schema
    .deriving(XmlFormat.deriver)
    .instance(TypeId.of[RawXml], RawXml.codec)
    .derive


  def main(args: Array[String]): Unit =
    val tei = TEI(
      teiHeader = TeiHeader(),
      text = Text(
        lang = Some("ru"),
        front = Some(Front()),
        body = RawXml(Xml.Element("body"))
      )
    )

    println(codec.encodeToString(tei, WriterConfig.pretty)) // TODO I will, of course, encode it to XML AST and write it myself ;)
