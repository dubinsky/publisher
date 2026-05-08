package org.podval.tools.publish

final class FontAwesome extends Html.JSLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[Html.Element] = List(Html.stylesheet(s"$cdn/css/all.min.css", idOpt = Some("fa-stylesheet")))
  override def body: List[Html.Element] = List.empty

object FontAwesome:
  import zio.blocks.html.*

  def brand(nameString: String): Html.Element =
    span(className := s"grey fa-brands fa-$nameString fa-lg")

  def icon(nameString: String, style: String): Html.Element =
    span(className := s"grey fa-classic fa-$style fa-$nameString")
