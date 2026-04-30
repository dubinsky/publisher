package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlBuilder, XmlName, XmlWriter}

// TODO split between Html and Xml
// TODO def list()...
object XmlUtil:
  extension (builder: XmlBuilder.ElementBuilder)
    def apply(children: Xml*): Xml.Element = builder.children(children *).build
    def withText(text: String): Xml.Element = builder.child(XmlBuilder.text(text)).build

    def setId(value: String): XmlBuilder.ElementBuilder =
      builder.attr(id, value)

    def setId(value: Option[String]): XmlBuilder.ElementBuilder = value match
      case None => builder
      case Some(id) => builder.setId(id)

    def child(child: Option[Xml]): XmlBuilder.ElementBuilder = child match
      case None => builder
      case Some(child) => builder.child(child)

  extension (element: Xml.Element)
    def getAttribute(name: XmlName): Option[String] =
      element.attributes.find(isAttribute(name)).map(_._2)
      
    def replaceAttribute(name: XmlName, value: String): Xml.Element =
      element.copy(attributes = element.attributes.filterNot(isAttribute(name)).appended(name -> value))
      
    def childrenWhenEmpty(text: Option[String]): Xml.Element =
      if element.children.nonEmpty then element
      else text.fold(element)(text => element.copy(children = Chunk(Xml.Text(text))))
  
  def el(name: String, attrs: (String, String)*): XmlBuilder.ElementBuilder =
    attrs.foldLeft(XmlBuilder.element(name))((result, attr) => result.attr(attr._1, attr._2))

  def div(cls: String): XmlBuilder.ElementBuilder = el("div", "class" -> cls)
  
  def a(cls: String, href: String): XmlBuilder.ElementBuilder = el("a", "class" -> cls, "href" -> href)
  
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private def isAttribute(name: XmlName)(attribute: (XmlName, String)): Boolean =
    attribute._1 == name
  
  def stylesheet(href: String, id: Option[String] = None): Xml.Element = el("link")
    .setId(id)
    .attr("rel", "stylesheet")
    .attr(hrefAttribute, href)
    .build

  def script(text: String): Xml.Element = el("script").withText(text)

  def module(src: String): Xml.Element = el("script")
    .attr("src", src)
    .child(XmlBuilder.comment("self-closing script elements do not work"))
    .build

  private def localName(name: String): XmlName = XmlName(name, None, None)

  val id: XmlName = localName("id")
  val a: XmlName = localName("a")
  val hrefAttribute: XmlName = localName("href")
  val `class`: XmlName = localName("class")
  val code: XmlName = localName("code")

  // Remove markup
  def toStringOpt(element: Xml.Element): Option[String] =
    Option.when(element.children.nonEmpty)(XmlUtil.toString(element))
    
  def toString(xml: Xml): String = xml match
    case Xml.Text(value) => value
    case Xml.Element(_, _, children) => children.map(toString).mkString(" ")
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

  // TODO I need to port my pretty-printer:
  // when I use ZIO Blocks' one with "pretty",
  // it inserts spaces between book titles and quotes around them -
  // and destroys the code blocks;
  // default one does not, but then the result is not pretty -
  // and I have to prevent some elements (script, data, span) from self-closing
  // by inserting comments into them, which is ugly.
  def write(xml: Xml): String = XmlWriter.write(xml/*, WriterConfig.pretty*/)

  abstract class JavascriptLibrary:
    def head: List[Xml.Element]
    def body: List[Xml.Element]
