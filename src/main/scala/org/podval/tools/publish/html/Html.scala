package org.podval.tools.publish.html

import org.podval.tools.publish.{LinksResolver, Markup, PageError, Path}
import zio.blocks.schema.xml.{Xml, XmlReader}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty
):
  override type AST = Xml
  
  override def parse(sourcePath: Path, content: String): Either[PageError, AST] =
    XmlReader.read(content) match
      case Right(xml) => Right(xml)
      case Left(error) => Left(PageError("Malformed HTML content", sourcePath, Some(error)))

  override def resolveLinks(xml: Xml, linkResolver: LinksResolver): Xml = xml // TODO
