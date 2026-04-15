package org.podval.tools.publish.html

import org.podval.tools.publish.{LinksResolver, Markup, PageError, Path}
import zio.blocks.schema.xml.{WriterConfig, Xml, XmlReader, XmlWriter}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty
):
  override type AST = Xml
  
  override def parse(sourcePath: Path, content: String): Either[PageError, Xml] =
    // TODO catch errors?
    Right(XmlReader.read(content))

  override def reformat(xml: Xml): Option[String] = None // TODO pretty-print

  override def resolveLinks(xml: Xml, linkResolver: LinksResolver): Xml = xml // TODO

  override def render(sourcePath: Path, xml: Xml): Either[PageError, Xml] = Right(xml)

  def write(xml: Xml): String = XmlWriter.write(xml, WriterConfig.pretty)
