package org.podval.tools.publish

final class FontAwesome extends Html.JSLibrary:
  val version: String = "7.0.0"
  private val cdn: String = s"https://cdn.jsdelivr.net/npm/@fortawesome/fontawesome-free@$version"
  override val head: List[Html.Element] = List(Html.stylesheet(s"$cdn/css/all.min.css", idOpt = Some("fa-stylesheet")))
  override def body: List[Html.Element] = List.empty

object FontAwesome:
  sealed abstract class Style(val name: String)

  object Style:
    object Solid extends Style("solid")
    object Regular extends Style("regular")

    private val all = Seq(Solid, Regular)
    
    def apply(name: Option[String]): Style = name.flatMap(name => all.find(_.name == name))
      .getOrElse:
        println(s"Unrecognized FontAwesome style: '$name'.")
        Solid
      
  import zio.blocks.html.*

  def brand(nameString: String): Html.Element =
    span(className := s"grey fa-brands fa-lg fa-$nameString")

  def icon(nameString: String, style: Style): Html.Element =
    span(className := s"grey fa-classic fa-${style.name} fa-$nameString")
