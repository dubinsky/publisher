package org.podval.tools.publish

import zio.blocks.schema.xml.XmlName

abstract class LinkElementResolver(
  val elementName: XmlName,
  val urlAttributeName: XmlName,
  val category: Option[String]
)

object LinkElementResolver:
  // TODO do 'img' too?

  object A extends LinkElementResolver(
    elementName = Html.a,
    urlAttributeName = Html.href,
    category = None
  )
  