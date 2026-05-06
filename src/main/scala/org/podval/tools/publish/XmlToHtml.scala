package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.html.Dom
import zio.blocks.schema.xml.{Xml, XmlName}

object XmlToHtml:
  private type HtmlElementMaker = (Chunk[Dom.Attribute], Chunk[Dom]) => Dom.Element

  def convertElement(element: Xml.Element): Dom.Element =
    htmlElementMaker(element.name)(
      element.attributes.map((name, value) => convertAttribute(name, value)),
      element.children.map(convert).filterNot(_.isInstanceOf[Dom.Empty.type])
    )

  private def convert(xml: Xml): Dom = xml match
    case Xml.Comment(value) => Dom.empty
    case Xml.ProcessingInstruction(target, data) => Dom.empty
    case Xml.Text(value) => Dom.text(value)
    case Xml.CData(value) => Dom.text(value)
    case element: Xml.Element => convertElement(element)

  private def htmlElementMaker(xmlName: XmlName): HtmlElementMaker = xmlName.qualifiedName match
    // TODO: recognize some known elements?
    case tag => Dom.Element.Generic(tag, _, _)

  private def convertAttribute(xmlName: XmlName, value: String): Dom.Attribute = xmlName.qualifiedName match
    // TODO: recognize some known attributes?
    case name => Dom.Attribute.KeyValue(name, Dom.AttributeValue.StringValue(value))
