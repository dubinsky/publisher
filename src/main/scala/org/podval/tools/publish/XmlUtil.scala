package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlBuilder, XmlName}

object XmlUtil:
  abstract class JavascriptLibrary:
    def head: List[BlocksHtml.Element]
    def body: List[BlocksHtml.Element]

  extension (builder: XmlBuilder.ElementBuilder)
    def apply(children: Xml*): Xml.Element = builder.children(children *).build

    def withText(text: String): Xml.Element = builder.child(XmlBuilder.text(text)).build

    def setId(value: String): XmlBuilder.ElementBuilder =
      builder.attr(idAttr, value)

    def setId(value: Option[String]): XmlBuilder.ElementBuilder = value match
      case None => builder
      case Some(id) => builder.setId(id)

    def attr(attr: Option[(String, String)]): XmlBuilder.ElementBuilder = attr match
      case None => builder
      case Some((name, value)) => builder.attr(name, value)

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

  def div(cls: String, attrs: (String, String)*): XmlBuilder.ElementBuilder = el("div", ("class" -> cls) +: attrs *)

  def spanXml(cls: String, attrs: (String, String)*): XmlBuilder.ElementBuilder = el("span", ("class" -> cls) +: attrs *)

  def ul[T](cls: String, values: Seq[T], li: T => Xml.Element): Xml.Element =
    el("ul", "class" -> cls)(values.map(value =>
      el("li")(li(value))
    )*)
    
  def a(cls: String, href: String): XmlBuilder.ElementBuilder = el("a", "class" -> cls, "href" -> href)
  
  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private def isAttribute(name: XmlName)(attribute: (XmlName, String)): Boolean =
    attribute._1 == name

  def stylesheet(hrefString: String, idOpt: Option[String] = None): BlocksHtml.Element =
    import zio.blocks.html.*
    link(rel := "stylesheet", href := hrefString).whenSome(idOpt)(idd => Seq(id := idOpt.get)) // TODO optional attributes??

  def faBrand(nameString: String): BlocksHtml.Element =
    import zio.blocks.html.*
    span(className := s"grey fa-brands fa-$nameString fa-lg")

  def faIcon(nameString: String): BlocksHtml.Element =
    import zio.blocks.html.*
    span(className := s"grey fa-classic fa-solid fa-$nameString")

  private def localName(name: String): XmlName = XmlName(name, None, None)

  val idAttr: XmlName = localName("id")
  val a: String = "a"
  val hrefAttribute: XmlName = localName("href")
  val `class`: XmlName = localName("class")
  val code: String = "code"
  
  def toId(text: String): String = text.trim.replace(' ', '-')

  def escapeText(text: String): String = Strings.escape(text)

  def escapeUrl(url: String): String = Strings.escape(url) // TODO

