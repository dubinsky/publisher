package org.podval.tools.publish

import zio.blocks.chunk.Chunk

// TODO rename Html
object BlocksHtml extends XmlAst:
  import zio.blocks.html.Dom as XML

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

  override def isAtom(xml: Xml): Boolean = xml match
    case _: XML.Text => true
    case _ => false

  override def atomText(xml: Xml): String = xml match
    case XML.Text(content) => content
    case xml => throw new IllegalArgumentException(s"Not an XML atom: $xml")
