package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlCodecError, XmlName, XmlReader, XmlWriter}

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
  
  def write(xml: Xml): String = XmlWriter.write(xml, WriterConfig.pretty)


  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private def isAttribute(name: XmlName)(attribute: (XmlName, String)): Boolean =
    attribute._1 == name
  
  def getAttribute(attributes: Chunk[(XmlName, String)], name: XmlName): Option[String] =
    attributes.find(isAttribute(name)).map(_._2)
    
  def replaceAttribute(attributes: Chunk[(XmlName, String)], name: XmlName, value: String): Chunk[(XmlName, String)] =
    attributes.filterNot(isAttribute(name)).appended(name -> value)
  