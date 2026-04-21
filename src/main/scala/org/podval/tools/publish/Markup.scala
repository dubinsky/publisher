package org.podval.tools.publish

import org.podval.tools.publish.Html.childWhen
import zio.blocks.schema.xml.{Xml, XmlName}
import scala.annotation.tailrec

object Markup:
  val all: List[Markup] = List(
    Markdown,
    Html
  )

abstract class Markup(
  final val extension: String,
  additionalExtensions: Set[String],
  final val noWikiLinksElements: Set[XmlName]
) derives CanEqual:
  override def toString: String = extension

  private val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def linkElementResolvers: Seq[LinkElementResolver]

  def findWikiLinks(xml: Xml): Xml = xml match
    case element: Xml.Element => findWikiLinksElement(element)
    case xml => xml
    
  private def findWikiLinksElement(element: Xml.Element): Xml =
    @tailrec
    def loop(result: Seq[Xml], text: String): Seq[Xml] = if text.isEmpty then result else
      findWikiLink(text, "![[", "]]", isEmbed = true).orElse(findWikiLink(text, "[[", "]]", isEmbed = false)) match
        case None => result ++ Seq(Xml.Text(text))
        case Some(before: String, wikiLink: Xml.Element, after: String) => loop(
          result = result ++ Option.when(before.nonEmpty)(Xml.Text(before)).toSeq ++ Seq(wikiLink),
          text = after
        )

    element match
      case Xml.Element(name, attributes, children) => Xml.Element(name, attributes, children.flatMap {
      case element: Xml.Element if !noWikiLinksElements.contains(element.name) => Seq(findWikiLinksElement(element))
      case Xml.Text(value) => loop(Seq.empty, value)
      case xml => Seq(xml)
    })

  private def findWikiLink(
    text: String,
    start: String,
    end: String,
    isEmbed: Boolean
  ): Option[(String, Xml.Element, String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (linkUrl: String, linkTextOpt: Option[String]) = Files.split(body, '|')
      val linkText: String = linkTextOpt.fold("")(_.trim)

      // TODO handle embedded images etc. right here
      val wikiLink: Xml.Element = Html.el(
          "a",
          "class" -> (if isEmbed then "embed" else "wiki-link"),
          "href" -> linkUrl.trim
        )
        .childWhen(linkText.nonEmpty, Xml.Text(linkText))
        .build

      Some(before, wikiLink, after)


