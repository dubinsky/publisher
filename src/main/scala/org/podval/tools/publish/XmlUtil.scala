package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlBuilder, XmlName, XmlWriter}

object XmlUtil:
  def el(name: String, attrs: (String, String)*): XmlBuilder.ElementBuilder =
    attrs.foldLeft(XmlBuilder.element(name))((result, attr) => result.attr(attr._1, attr._2))

  def div(cls: String): XmlBuilder.ElementBuilder = el("div", "class" -> cls)
  
  extension (builder: XmlBuilder.ElementBuilder)
    def apply(children: Xml*): Xml.Element = builder.children(children *).build
    def apply(text: String): Xml.Element = builder.child(XmlBuilder.text(text)).build

    def attrWhen(when: Boolean, name: String, value: => String): XmlBuilder.ElementBuilder =
      if !when then builder else builder.attr(name, value)

    def childWhen(when: Boolean, child: => Xml): XmlBuilder.ElementBuilder =
      if !when then builder else builder.child(child)

    def childrenWhen(when: Boolean, children: => Seq[Xml]): XmlBuilder.ElementBuilder =
      if !when then builder else builder.children(children *)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private def isAttribute(name: XmlName)(attribute: (XmlName, String)): Boolean =
    attribute._1 == name

  def getAttribute(attributes: Chunk[(XmlName, String)], name: XmlName): Option[String] =
    attributes.find(isAttribute(name)).map(_._2)

  def replaceAttribute(attributes: Chunk[(XmlName, String)], name: XmlName, value: String): Chunk[(XmlName, String)] =
    attributes.filterNot(isAttribute(name)).appended(name -> value)
  
  def stylesheet(href: String, id: Option[String] = None): Xml.Element = XmlBuilder
    .element("link")
    .attrWhen(id.nonEmpty, "id", id.get)
    .attr("rel", "stylesheet")
    .attr("href", href)
    .build

  def script(text: String): Xml.Element = XmlBuilder
    .element("script")
    .child(XmlBuilder.text(text))
    .build

  def module(src: String): Xml.Element = XmlBuilder
    .element("script")
    .attr("src", src)
    .child(XmlBuilder.comment("self-closing script elements do not work"))
    .build

  private def localName(name: String): XmlName = XmlName(name, None, None)

  val id: XmlName = localName("id")
  val a: XmlName = localName("a")
  val href: XmlName = localName("href")
  val `class`: XmlName = localName("class")
  val code: XmlName = localName("code")

  // Remove markup
  def toSimpleString(xml: Xml): String = xml match
    case Xml.Text(value) => value
    case Xml.Element(_, _, children) => children.map(toSimpleString).mkString(" ")
    case xml => ""

  def toId(text: String): String = text.trim.replace(' ', '-')

  def escapeText(text: String): String = escape(text)

  def escapeUrl(url: String): String = escape(url) // TODO

  private def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")

  // TODO port my pretty-printer; make sure that elements do not self-close (script, data, span).
  def write(xml: Xml): String = XmlWriter.write(xml, WriterConfig.pretty)

  abstract class JavascriptLibrary:
    def head: List[Xml.Element]

    def body: List[Xml.Element]
