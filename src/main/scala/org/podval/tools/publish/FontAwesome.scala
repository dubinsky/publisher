package org.podval.tools.publish

final class FontAwesome extends Html.JSLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[Html.Element] = List(Html.stylesheet(s"$cdn/css/all.min.css", idOpt = Some("fa-stylesheet")))
  override def body: List[Html.Element] = List.empty

object FontAwesome:
  import zio.blocks.html.*

  val folder = Icon("folder", Regular)
  val note = Icon("note-sticky", Regular)
  val envelope = Icon("envelope", Regular)
  val tags = Icon("tags", Solid)
  val errors = Icon("circle-xmark", Regular)
  val arrowUp = Icon("arrow-up", Solid)
  val arrowLeft = Icon("arrow-left", Solid)
  val arrowRight = Icon("arrow-right", Solid)
  val rss = Icon("rss", Solid)
  def brand(name: String) = Icon(name, Brands)

  final class Icon(val name: String, val style: Style):
    /* TODO! that did not work; probably lost in XmlToHtml..., className += s"fa-$name"*/
    def htmlSpan: Html.Element = span(className := s"${style.classNames} fa-$name")
  
  sealed abstract class Style:
    final def classNames: String = s"grey $additions fa-$name"
    def name: String
    def additions: String

  case object Solid extends Style:
    override def name: String = "solid"
    override def additions: String = "fa-classic"

  case object Regular extends Style:
    override def name: String = "regular"
    override def additions: String = "fa-classic"

  case object Brands extends Style:
    override def name: String = "brands"
    override def additions: String = "fa-lg"
