package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom as XML
import zio.blocks.schema.xml.Xml as From

object Html extends XmlAst:
  override type Xml = XML
  override type Element = XML.Element

  override def isElement(xml: Xml): Boolean = xml match
    case _: XML.Element => true
    case _ => false

  override def asElement(xml: Xml): Element = xml.asInstanceOf[XML.Element]

  // TODO ZIO Blocks HTML does not allow prefixed names?!
  override def qName(element: Element): String = element.tag

  override def children(element: Element): Chunk[Xml] = element.children

  override def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)] =
    element.attributes.map {
      case XML.Attribute.KeyValue(name, value) => (name, attributeValue(value))
      case XML.Attribute.BooleanAttribute(name, enabled) => (name, enabled.toString) // TODO!
      case XML.Attribute.AppendValue(name, value, separator) => (name, attributeValue(value))
    }

  private def attributeValue(value: XML.AttributeValue): String = value match
    case XML.AttributeValue.StringValue(value) => value
    case XML.AttributeValue.BooleanValue(value) => value.toString // TODO!
    case XML.AttributeValue.MultiValue(values, separator) => values.mkString(separator.render)
    case XML.AttributeValue.JsValue(value) => value.value

  override def mkText(text: String): Xml = XML.text(text)

  override def isText(xml: Xml): Boolean = isAtom(xml)

  override def isAtom(xml: Xml): Boolean = xml match
    case _: XML.Text => true
    case _ => false

  override def atomText(xml: Xml): String = xml match
    case XML.Text(content) => content
    case xml => throw new IllegalArgumentException(s"Not an XML atom: $xml")

  abstract class JSLibrary:
    def head: List[Html.Element]
    def body: List[Html.Element]

  // Note: I do not see any reason to recognize elements (like 'script') or attributes (like 'hidden')...
  def fromXml(element: From.Element): XML.Element = XML.Element.Generic(
    tag = element.name.qualifiedName,
    attributes = element.attributes.map((name, value) => XML.Attribute.KeyValue(
      name.qualifiedName,
      XML.AttributeValue.StringValue(value)
    )),
    children = element.children.flatMap {
      case From.Comment(value) => None
      case From.ProcessingInstruction(target, data) => None
      case From.Text(value) => Some(mkText(value))
      case From.CData(value) => Some(mkText(value))
      case element: From.Element => Some(fromXml(element))
    }
  )

  // for convenience

  val a: String = "a"
  val code: String = "code"
  val hrefAttr: String = "href"

  def stylesheet(
    hrefString: String,
    idOpt: Option[String] = None
  ): Html.Element =
    import zio.blocks.html.*

    link(rel := "stylesheet", href := hrefString).whenSome(idOpt)(idd => Seq(id := idOpt.get)) // TODO optional attributes??
