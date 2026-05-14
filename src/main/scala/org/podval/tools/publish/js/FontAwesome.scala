package org.podval.tools.publish.js

import org.podval.xml.Html

object FontAwesome extends JSLibrary:
  val version: String = "7.0.0"

  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[Html.Element] = List(Html.stylesheet(s"$cdn/css/all.min.css", idOpt = Some("fa-stylesheet")))

  override def body: List[Html.Element] = List.empty
