package org.podval.tools.publish

final class FontAwesome extends XmlUtil.JavascriptLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[BlocksHtml.Element] = List(XmlUtil.stylesheet(s"$cdn/css/all.min.css", idOpt = Some("fa-stylesheet")))
  override def body: List[BlocksHtml.Element] = List.empty
