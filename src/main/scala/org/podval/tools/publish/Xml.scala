package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml as XML, XmlBuilder, XmlName}

object Xml extends XmlAst:
  override type Xml = XML
  override type Element = XML.Element

  override def asElement(xml: Xml): Option[Element] = xml match
    case element: XML.Element => Some(element)
    case _ => None

  override def qName(element: Element): String = element.name.qualifiedName

  override def element(name: String): Element = XmlBuilder.element(name).build

  override def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)] =
    attributes(element)

  override def attributes(element: Element): Chunk[(String, String)] =
    element.attributes.map((xmlName, value) => (xmlName.qualifiedName, value))

  override def setAttribute(element: Element, name: String, value: String): Element =
    element.copy(attributes = element.attributes
      .filterNot(_._1.qualifiedName == name)
      .appended(XmlName(name) -> value)
    )

  override def children(element: Element): Chunk[Xml] = element.children

  override def setChildren(element: Element, children: Chunk[Xml]): Element =
    element.copy(children = children)

  override def mkText(text: String): Xml = XML.Text(text)

  override def asText(xml: Xml): Option[String] = xml match
    case XML.Text(value) => Some(value)
    case _ => None

  override def asAtom(xml: Xml): Option[String] = xml match
    case XML.Text(value) => Some(value)
    case XML.CData(value) => Some(value)
    case _ => None


