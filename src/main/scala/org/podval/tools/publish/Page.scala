package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec

final class Page(
  val sourcePath: Path,
  targetPath: Path,
  pageKind: PageKind,
  val markup: Markup,
  frontMatter: FrontMatter,
  xmlRaw: Xml.Element
) extends PageBase(
  targetPath,
  pageKind,
  frontMatter
):
  override def toString: String = s"$title.$markup($sourcePath, $targetPath)"

  private var xmlVar: Xml.Element = markup.dropAnchors(xmlRaw)
  override def xml: Xml.Element = xmlVar
  
  override def paths: List[Path] = List(
    sourcePath,
    sourcePath.withoutExtension,
    targetPath,
    targetPath.withoutExtension
  )
  
  // TODO add TOC to page as needed

  private val sections: Seq[Section] = markup.sections(xml)

  private val sectionsFlat: Seq[Section] = sections.flatMap(_.sectionsFlat)

  def section(names: Seq[String]): Option[Seq[Section]] =
    def loop(result: Seq[Section], sections: Seq[Section], names: Seq[String]): Option[Seq[Section]] =
      if names.isEmpty then Some(result) else sections.find(_.is(names.head)).flatMap(section => loop(
        result = result :+ section,
        sections = section.sections,
        names = names.tail
      ))

    loop(
      result = Seq.empty,
      sections = sectionsFlat,
      names = names
    )

  def block(id: String): Option[Block] = None // TODO
  
  def resolveLinks(site: Site): Unit =
    def loop(element: Xml.Element): Xml.Element =
      if !markup.resolveLinks(element) then element else
        val result: Xml.Element = element.copy(children = element.children.flatMap {
          case element: Xml.Element => Seq(loop(element))
          case Xml.Text(value) if markup.resolveWikiLinks => resolveWikiLinks(value, site)
          case xml => Seq(xml)
        })
        markup.linkFromElement(result) match
          case None => result
          case Some(linkFromElement) => site.resolveLink(Link.From(
            page = this,
            fromElement = linkFromElement,
            context = None,
            element = Some(element),
            transclude = false
          ))

    xmlVar = loop(xml)

  // see https://obsidian.md/help/links
  private def resolveWikiLinks(text: String, site: Site): Seq[Xml] =
    @tailrec
    def loop(result: Seq[Xml], text: String): Seq[Xml] =
      if text.isEmpty then result else
        resolveWikiLink(text, "![[", "]]", transclude = true, site)
          .orElse(resolveWikiLink(text, "[[", "]]", transclude = false, site)) match
            case None => result ++ Seq(Xml.Text(text))
            case Some(xml: Seq[Xml], after: String) => loop(result ++ xml, after)

    loop(Seq.empty, text)

  private def resolveWikiLink(
    text: String,
    start: String,
    end: String,
    transclude: Boolean,
    site: Site
  ): Option[(Seq[Xml], String)] =
    val startIndex: Int = text.indexOf(start)
    val endIndex: Int = if startIndex == -1 then -1 else text.indexOf(end, startIndex + start.length)
    if endIndex == -1 then None else
      val before: String = text.substring(0, startIndex).trim
      val body: String = text.substring(startIndex + start.length, endIndex).trim
      val after: String = text.substring(endIndex + end.length).trim
      val (ref: String, textOpt: Option[String]) = Files.split(body, '|')

      val result: Xml.Element = site.resolveLink(Link.From(
        page = this,
        fromElement = Link.FromElement(
          ref = Some(ref.trim),
          text = textOpt.map(_.trim),
          kind = None,
        ),
        context = None,
        element = None,
        transclude = transclude
      ))

      Some(
        Option.when(before.nonEmpty)(Xml.Text(before)).toSeq ++ Seq(result),
        after
      )
