package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}

abstract class LinkElementResolver(
  val elementName: XmlName,
  val category: Option[String]
):
  def url(attributes: Chunk[(XmlName, String)]): Option[String]

  def updateAttributes(
    attributes: Chunk[(XmlName, String)],
    urlResolved: String
  ): Chunk[(XmlName, String)]
  
  def updateChildren(
    children: Chunk[Xml],
    textResolved: String
  ): Chunk[Xml]

object LinkElementResolver:
  // TODO do 'img' too?

  object A extends LinkElementResolver(
    elementName = XmlName("a", None, None),
    category = None
  ):
    private given CanEqual[XmlName, XmlName] = CanEqual.derived

    private val hrefName: XmlName = XmlName("href", None, None)

    private def isHref(attr: (XmlName, String)) = attr._1 == hrefName
    
    override def url(attributes: Chunk[(XmlName, String)]): Option[String] =
      attributes.find(isHref).map(_._2)

    override def updateAttributes(
      attributes: Chunk[(XmlName, String)],
      urlResolved: String
    ): Chunk[(XmlName, String)] =
      attributes.filterNot(isHref).appended(hrefName -> urlResolved)

    def updateChildren(
      children: Chunk[Xml],
      textResolved: String
    ): Chunk[Xml] =
      if !children.isEmpty then children else Chunk(Xml.Text(textResolved))
