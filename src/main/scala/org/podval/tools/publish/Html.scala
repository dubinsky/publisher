package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlBuilder, XmlCodecError, XmlName, XmlReader, XmlWriter}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty
):
  override def parse(sourcePath: Path, content: String): Either[PageError, Xml] =
    try Right(XmlReader.read(content).asInstanceOf[Xml.Element])
    catch case e: XmlCodecError => Left(PageError(sourcePath, e.getMessage))

  override def linkElementResolvers: Seq[LinkElementResolver] = Seq(
    LinkElementResolver.A
  )

  // TODO port my pretty-printer; make sure that elements do not self-close (script, data, span).
  def write(xml: Xml): String = XmlWriter.write(xml, WriterConfig.pretty)

  def el(name: String, attrs: (String, String)*): XmlBuilder.ElementBuilder =
    attrs.foldLeft(XmlBuilder.element(name))((result, attr) => result.attr(attr._1, attr._2))

  extension (builder: XmlBuilder.ElementBuilder)
    def apply(children: Xml*): Xml.Element = builder.children(children *).build
    def apply(text: String): Xml.Element = builder.child(XmlBuilder.text(text)).build
    
    def childWhen(when: Boolean, child: => Xml): XmlBuilder.ElementBuilder =
      if !when then builder else builder.child(child)

    def childrenWhen(when: Boolean, children: => Seq[Xml]): XmlBuilder.ElementBuilder =
      if !when then builder else builder.children(children*)

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private def isAttribute(name: XmlName)(attribute: (XmlName, String)): Boolean =
    attribute._1 == name

  def getAttribute(attributes: Chunk[(XmlName, String)], name: XmlName): Option[String] =
    attributes.find(isAttribute(name)).map(_._2)

  def replaceAttribute(attributes: Chunk[(XmlName, String)], name: XmlName, value: String): Chunk[(XmlName, String)] =
    attributes.filterNot(isAttribute(name)).appended(name -> value)

  val a: XmlName = XmlName("a", None, None)
  val href: XmlName = XmlName("href", None, None)
  val `class`: XmlName = XmlName("class", None, None)
  val code: XmlName = XmlName("code", None, None)

  def escapeText(text: String): String = escape(text)
  def escapeUrl(url: String): String = escape(url) // TODO

  private def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")
