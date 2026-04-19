package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}

abstract class JavascriptLibrary:
  def head: List[Xml.Element]
  def body: List[Xml.Element]

  final protected def stylesheet(href: String): Xml.Element = XmlBuilder
    .element("link")
    .attr("rel", "stylesheet")
    .attr("href", href)
    .build

  final protected def script(text: String): Xml.Element = XmlBuilder
    .element("script")
    .child(XmlBuilder.text(text))
    .build

  final protected def module(src: String): Xml.Element = XmlBuilder
    .element("script")
    .attr("src", src)
    .child(XmlBuilder.comment("self-closing script elements do not work"))
    .build
