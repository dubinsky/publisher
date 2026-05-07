package org.podval.tools.publish

import scala.annotation.tailrec

// see https://obsidian.md/help/links
object WikiLink:
  @tailrec
  def resolveWikiLinks(result: Seq[Xml.Xml], text: String, page: MarkupPage): Seq[Xml.Xml] =
    if text.isEmpty then result else
      resolveWikiLink(text, "![[", "]]", transclude = true, page)
        .orElse(resolveWikiLink(text, "[[", "]]", transclude = false, page)) match
        case None => result ++ Seq(Xml.mkText(text))
        case Some(xml: Seq[Xml.Xml], after: String) => resolveWikiLinks(result ++ xml, after, page)

  private def resolveWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean,
    page: MarkupPage
  ): Option[(Seq[Xml.Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Strings.split(body, '|')

      val result: Xml.Element = page.site.resolveLink(Link(
        page = page,
        element = None,
        ref = ref.trim,
        text = textOpt.map(_.trim),
        kind = None,
        context = None,
        transclude = transclude
      ))

      Some(
        Option.when(before.nonEmpty)(Xml.mkText(before)).toSeq ++ Seq(result),
        after
      )
  