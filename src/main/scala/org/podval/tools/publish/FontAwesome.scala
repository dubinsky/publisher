package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final class FontAwesome extends XmlUtil.JavascriptLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[Xml.Element] = List(XmlUtil.stylesheet(s"$cdn/css/all.min.css", id = Some("fa-stylesheet")))
  override def body: List[Xml.Element] = List.empty
