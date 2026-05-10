package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml as XML, XmlBuilder, XmlName}

object Xml extends XmlAst:
  override type Xml = XML
  override type Element = XML.Element

  override def asElement(xml: Xml): Element = xml.asInstanceOf[XML.Element]

  override def qName(element: Element): String = element.name.qualifiedName
  
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

  override def isElement(xml: Xml): Boolean = xml match
    case _: XML.Element => true
    case _ => false

  override def isText(xml: Xml): Boolean = xml match
    case _: XML.Text => true
    case _ => false

  override def isAtom(xml: Xml): Boolean = xml match
    case _: XML.Text => true
    case _: XML.CData => true
    case _ => false

  override def atomText(xml: Xml): String = xml match
    case XML.Text(value) => value
    case XML.CData(value) => value
    case xml => throw new IllegalArgumentException(s"Not an XML atom: $xml")

  // for convenience
  
  def element(name: String): XmlBuilder.ElementBuilder = XmlBuilder.element(name)


