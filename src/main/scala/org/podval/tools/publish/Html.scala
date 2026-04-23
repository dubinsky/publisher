package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlCodecError, XmlName, XmlReader}

object Html extends HtmlLike:
  override val extension: String = "html"
  override val additionalExtensions: Set[String] = Set.empty
  
  override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
    try Right(XmlReader.read(content).asInstanceOf[Xml.Element])
    catch case e: XmlCodecError => PageError.Parsing(sourcePath, "", Some(e))
