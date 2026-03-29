package org.podval.tools.publish.html

import org.podval.tools.publish.{LinksResolver, Markup, PageError, Path}
import zio.blocks.schema.xml.{Xml, XmlReader, XmlWriter}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty
):
  override type AST = Xml
  
  override def parse(sourcePath: Path, content: String): Either[PageError, AST] =
    XmlReader.read(content) match
      case Right(xml) => Right(xml)
      case Left(error) => Left(PageError(sourcePath, "Malformed HTML", Some(error)))

  override def reformat(xml: Xml): Option[String] = None // TODO pretty-print

  override def resolveLinks(xml: Xml, linkResolver: LinksResolver): Xml = xml // TODO

  override def render(xml: Xml): Xml = xml

  def write(xml: Xml): String = XmlWriter.write(xml)
