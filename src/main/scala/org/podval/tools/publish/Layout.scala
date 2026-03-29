package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

trait Layout:
  // TODO add backlinks and some stuff from the FrontMatter
  def render(content: Xml, backLinks: List[Link]): Xml.Element

object Layout:
  object Default extends Layout:
    // TODO implement!
    override def render(content: Xml, backLinks: List[Link]): Xml.Element = Xml.Element("div")
