package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Alignment, Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Emphasis, HardBreak,
  Heading, HtmlBlock, HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, Parser, SoftBreak,
  Strikethrough, Strong, Table, TableRow, Text, ThematicBreak}
import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec
import Html.{escapeText, escapeUrl}

object Markdown extends Markup(
  extension = "md",
  additionalExtensions = Set.empty
):
  override def linkElementResolvers: Seq[LinkElementResolver] = Seq(
    LinkElementResolver.A
  )

  private given CanEqual[Alignment, Alignment] = CanEqual.derived
  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived
  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

  // Based on `zio.blocks.docs.HtmlRenderer`:
  // - translated to Scala 3 syntax;
  // - removed code duplication;
  // - closed 'hr', 'br', 'input' and 'img' tags;
  // - NOT removed escaping of Text/Inline.Text;
  // - changed '"&#39;' to '"&apos;';
  // - attributes must have values ('checked', 'disabled');
  // - detect wiki links and render them;
  override def parse(sourcePath: Path, content: String): Either[PageError, Xml] = Parser.parse(content) match
    case Left(error) => Left(PageError(sourcePath, s"Malformed Markdown: $error"))
    case Right(doc) => Html.parse(sourcePath, s"<div>${renderBlocks(doc.blocks)}</div>")

  private def renderBlocks(blocks: Chunk[Block]): String =
    blocks.map(renderBlock).mkString

  private def renderBlocks(tag: String, blocks: Chunk[Block], attrs: String = ""): String =
    s"<$tag$attrs>${renderBlocks(blocks)}</$tag>"

  private def renderBlock(block: Block): String = block match
    case Paragraph(content) => renderInlines("p", content)
    case Heading(level, content) => renderInlines(s"h${level.value}", content)
    case CodeBlock(info, code) => s"<pre>${renderCode(code, info)}</pre>"
    case ThematicBreak => "<hr/>"
    case BlockQuote(content) => renderBlocks("blockquote", content)
    case BulletList(items, _) => renderBlocks("ul", items)
    case OrderedList(start, items, _) => renderBlocks("ol", items, attrs = if start == 1 then "" else s""" start="$start"""")
    case ListItem(content, checked) => s"<li>${renderInput(checked)}${renderBlocks(content)}</li>"
    case HtmlBlock(html) => html

    case Table(header, alignments, rows) =>
      def renderTableRow(tag: String, row: TableRow): String = row
        .cells
        .zip(alignments)
        .map { case (cell, alignment) => renderInlines(tag, cell, attrs = renderAlignment(alignment)) }
        .mkString("<tr>", "", "</tr>")

      val headerHtml = s"<thead>${renderTableRow("th", header)}</thead>"
      val bodyHtml = s"<tbody>${rows.map(row => renderTableRow("td", row)).mkString}</tbody>"
      s"<table>$headerHtml$bodyHtml</table>"

  private def renderInlines(tag: String, inlines: Chunk[Inline], attrs: String = ""): String =
    s"<$tag$attrs>${inlines.map(renderInline).mkString}</$tag>"

  private def renderInline(inline: Inline): String = inline match
    case Text(value) => renderText(value)
    case Inline.Text(value) => renderText(value)

    case Code(value) => renderCode(value, language = None)
    case Inline.Code(value) => renderCode(value, language = None)

    case Emphasis(content) => renderInlines("em", content)
    case Inline.Emphasis(content) => renderInlines("em", content)

    case Strong(content) => renderInlines("strong", content)
    case Inline.Strong(content) => renderInlines("strong", content)

    case Strikethrough(content) => renderInlines("del", content)
    case Inline.Strikethrough(content) => renderInlines("del", content)

    case Link(text, url, titleOpt) => renderLink(text, url, titleOpt)
    case Inline.Link(text, url, titleOpt) => renderLink(text, url, titleOpt)

    case Image(alt, url, titleOpt) => renderImage(alt, url, titleOpt)
    case Inline.Image(alt, url, titleOpt) => renderImage(alt, url, titleOpt)

    case HtmlInline(html) => html
    case Inline.HtmlInline(html) => html

    case SoftBreak => " "
    case Inline.SoftBreak => " "

    case HardBreak => "<br/>"
    case Inline.HardBreak => "<br/>"

    case Autolink(url, isEmail) => renderAutoLink(url, isEmail)
    case Inline.Autolink(url, isEmail) => renderAutoLink(url, isEmail)

  private def renderCode(code: String, language: Option[String]): String =
    val classAttr: String = language.fold("")(lang => s""" class="language-$lang"""")
    s"<code$classAttr>${escapeText(code)}</code>"
    
  private def renderLink(text: Chunk[Inline], url: String, titleOpt: Option[String]) =
    renderInlines("a", text, attrs = s""" href="${escapeUrl(url)}"${title(titleOpt)}""")

  private def renderImage(alt: String, url: String, titleOpt: Option[String]) =
    s"""<img src="${escapeUrl(url)}" alt="${escapeText(alt)}"${title(titleOpt)}/>"""
  
  private def renderAutoLink(url: String, isEmail: Boolean) =
    s"""<a href="${if isEmail then "mailto:" else ""}${escapeUrl(url)}">${escapeUrl(url)}</a>"""

  private def renderInput(checked: Option[Boolean]): String = checked.fold("")(checked =>
    val checkedAttr = if checked then """ checked="true"""" else ""
    s"""<input type="checkbox"$checkedAttr disabled="true"/>"""
  )

  private def renderAlignment(alignment: Alignment): String = alignment match
    case Alignment.Left => s""" style="text-align:left""""
    case Alignment.Right => s""" style="text-align:right""""
    case Alignment.Center => s""" style="text-align:center""""
    case Alignment.None => s""
    
  private def title(titleOpt: Option[String]): String =
    titleOpt.fold("")(title => s"title=${escapeText(title)}")

  // Processes wiki links.
  private def renderText(text: String) =
    @tailrec
    def loop(result: String, text: String): String =
      if text.isEmpty then result else
        val start: Int = text.indexOf("[[")
        val end: Int = if start == -1 then -1 else text.indexOf("]]", start + 2)
        if end == -1
        then escapeText(text)
        else loop(
          result = result ++
            escapeText(text.substring(0, start)) ++
            renderWikiLink(text.substring(start + 2, end)),
          text = text.substring(end + 2)
        )

    loop("", text)

  private def renderWikiLink(body: String): String =
    val (url: String, text: Option[String]) = Files.split(body.trim, '|')
    s"""<a class="wiki-link" href="${escapeUrl(url)}">${text.fold("")(text => escapeText(text))}</a>"""
