package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlCodecError, XmlName, XmlReader}

object Html extends Markup(
  extension = "html",
  additionalExtensions = Set.empty,
  noWikiLinksElements = Set(XmlName("code", None, None))
):
  override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
    try Right(XmlReader.read(content).asInstanceOf[Xml.Element])
    catch case e: XmlCodecError => Left(PageError(sourcePath, e.getMessage))

  // TODO do 'img' too?

  private object ALinkElementResolver extends Link.ElementResolver(
    elementName = XmlUtil.a,
    urlAttributeName = XmlUtil.href,
    category = None
  )
  
  override def linkElementResolvers: Seq[Link.ElementResolver] = Seq(
    ALinkElementResolver
  )
