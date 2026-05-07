package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml as XML, XmlBuilder, XmlName}

object Xml extends XmlAst:
  override type Xml = XML
  override type Element = XML.Element

  override def asElement(xml: Xml): Element = xml.asInstanceOf[XML.Element]

  override def qName(element: Element): String = element.name.qualifiedName

  override def children(element: Element): Chunk[Xml] = element.children

  override def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)] =
    element.attributes.map((xmlName, value) => (xmlName.qualifiedName, value))

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

  val idAttr: String = "id"

  def toId(text: String): String = text.trim.replace(' ', '-')

  // for convenience

  def element(name: String): XmlBuilder.ElementBuilder = XmlBuilder.element(name)

  def a(
    cls: String,
    href: String,
    text: String
  ): Xml.Element = Xml
    .element("a")
    .attr("class", cls)
    .attr(Html.hrefAttr, href)
    .child(Xml.mkText(text))
    .build

  extension (element: Xml.Element)
    private def isAttribute(name: String)(attribute: (XmlName, String)): Boolean =
      attribute._1.qualifiedName == name

    def getAttribute(name: String): Option[String] = element.attributes
      .find(isAttribute(name)).map(_._2)

    def replaceAttribute(name: String, value: String): Xml.Element = element.copy(attributes = element.attributes
      .filterNot(isAttribute(name))
      .appended(XmlName(name) -> value)
    )
