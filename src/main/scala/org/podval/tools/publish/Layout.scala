package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}

trait Layout:
  // TODO add backlinks and some stuff from the FrontMatter
  def render(content: Xml, backLinks: List[Link]): Xml

object Layout:
  object Default extends Layout:
    override def render(content: Xml, backLinks: List[Link]): Xml = XmlBuilder
      .element("html")
      .attr("lang", "en") // TODO from the FrontMatter
      .child(XmlBuilder
        .element("head")
        .child(XmlBuilder
          .element("title")
          .child(XmlBuilder.text("TiTlE")) // TODO from the page
          .build
        )
        .child(XmlBuilder
          .element("link")
          .attr("rel", "stylesheet")
          .attr("href", "/assets/site.css")
          .build
        )
        .build
      )
      .child(XmlBuilder
        .element("body")
        .child(XmlBuilder
          .element("main")
          .child(content)
          .build
        )
        .build
      )
      .build
