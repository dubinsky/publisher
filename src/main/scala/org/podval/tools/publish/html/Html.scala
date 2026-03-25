package org.podval.tools.publish.html

import org.podval.tools.publish.Markup
import org.slf4j.{Logger, LoggerFactory}
import zio.blocks.schema.xml.{Xml, XmlError, XmlReader}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty
):
  override type AST = Xml

  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  override def read(content: String): AST =
    XmlReader.read(content) match
      case Right(xml) => xml
      case Left(error) =>
        log.error("XML parsing error", error)
        Xml.Element.empty

