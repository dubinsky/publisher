package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlCodecError, XmlReader, XmlWriter, WriterConfig}

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
