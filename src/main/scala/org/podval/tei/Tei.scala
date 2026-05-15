package org.podval.tei

import org.podval.xml.RawXml
import zio.blocks.schema.{Modifier, Schema}
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlCodec, XmlFormat}
import zio.blocks.typeid.TypeId

// TODO there seems to be no way to make the top TEI element to be `TEI` and not `tei` - who name-maps it?
// may have to code the top-level codec myself...

// TODO @xmlAttribute("id") and @xmlNamespace() annotations mentioned in the documentation
// do not work! Raw @Modifier.config("xml.attribute", "") does...
// File te issue.

@Modifier.config("xml.namespace.uri", "http://www.tei-c.org/ns/1.0")
@Modifier.config("xml.namespace.prefix", "tei")
final class Tei(
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

  given schema: Schema[Tei] = Schema.derived

  private val codec: XmlCodec[Tei] = schema
    .deriving(XmlFormat.deriver)
    .instance(TypeId.of[RawXml], RawXml.codec)
    .derive


  def main(args: Array[String]): Unit =
    val tei = Tei(
      teiHeader = TeiHeader(),
      text = Text(
        lang = Some("ru"),
        front = Some(Front()),
        body = RawXml(Xml.Element("body"))
      )
    )

    println(codec.encodeToString(tei, WriterConfig.pretty)) // TODO I will, of course, encode it to XML AST and write it myself ;)
