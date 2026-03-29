package org.podval.tools.publish.markdown

import org.podval.tools.publish.{Files, LinkResolved, LinksResolver}
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Doc, Emphasis, Heading, HtmlBlock,
  HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, Renderer, Strikethrough, Strong, Table, TableRow,
  Text, ThematicBreak}
import scala.annotation.tailrec

final class MarkdownLinksResolver(linkResolver: LinksResolver):
  def resolveDoc(doc: Doc): Doc = Doc(metadata = doc.metadata, blocks = resolveBlocks(doc.blocks))

  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived

  private def resolveBlocks(blocks: Chunk[Block]): Chunk[Block] = blocks.map(resolveBlock)

  private def resolveBlock(block: Block): Block = block match
    case Paragraph(content) => Paragraph(resolveInlines(content))
    case Heading(level, content) => Heading(level, resolveInlines(content))
    case CodeBlock(info, code) => CodeBlock(info, code)
    case ThematicBreak => ThematicBreak
    case BlockQuote(content) => BlockQuote(resolveBlocks(content))
    case BulletList(items, tight) => BulletList(resolveListItems(items), tight)
    case OrderedList(start, items, tight) => OrderedList(start, resolveListItems(items), tight)
    case ListItem(content, checked) => resolveListItem(ListItem(content, checked))
    case HtmlBlock(content) => HtmlBlock(content)
    case Table(header, alignments, rows) => Table(resolveTableRow(header), alignments, rows.map(resolveTableRow))

  private def resolveListItems(items: Chunk[ListItem]): Chunk[ListItem] = items.map(resolveListItem)
  private def resolveListItem(item: ListItem): ListItem = ListItem(resolveBlocks(item.content), item.checked)
  private def resolveTableRow(row: TableRow): TableRow = TableRow(row.cells.map(resolveInlines))

  private def resolveInlines(inlines: Chunk[Inline]): Chunk[Inline] =
    Markdown.splitIntoLines(inlines).flatMap: line =>
      val context: String = Renderer.renderInlines(line).trim // TODO render to HTML string?
      line.flatMap:
        case Text(value) => resolveText(value, context, Text.apply)
        case Inline.Text(value) => resolveText(value, context, Inline.Text.apply)
        case inline: Inline => Chunk:
          inline match
            case Link(text, url, title) => Link(text, resolveLink(url, context), title)
            case Inline.Link(text, url, title) => Inline.Link(text, resolveLink(url, context), title)
            case Image(alt, url, title) => Image(alt, url, title) // TODO resolve URL?
            case Inline.Image(alt, url, title) => Inline.Image(alt, url, title)
            case Emphasis(content) => Emphasis(resolveInlines(content))
            case Inline.Emphasis(content) => Inline.Emphasis(resolveInlines(content))
            case Strong(content) => Strong(resolveInlines(content))
            case Inline.Strong(content) => Inline.Strong(resolveInlines(content))
            case Strikethrough(content) => Strikethrough(resolveInlines(content))
            case Inline.Strikethrough(content) => Inline.Strikethrough(resolveInlines(content))
            case Code(value) => Code(value)
            case Inline.Code(value) => Inline.Code(value)
            case HtmlInline(content) => HtmlInline(content)
            case Inline.HtmlInline(content) => Inline.HtmlInline(content)
            case Autolink(url, isEmail) => Autolink(url, isEmail)
            case Inline.Autolink(url, isEmail) => Inline.Autolink(url, isEmail)
            case inline if Markdown.isBreak(inline) => inline
            case _ => throw IllegalStateException("Can not happen!")

  private def resolveText(
    text: String,
    context: String,
    textMaker: String => Inline
  ): Chunk[Inline] =
    @tailrec
    def loop(text: String, result: Chunk[Inline]): Chunk[Inline] =
      if text.isEmpty then result else
        val start: Int = text.indexOf("[[")
        val end: Int = if start == -1 then -1 else text.indexOf("]]", start+2)
        if end == -1
        then loop("", result :+ textMaker(text))
        else
          val before: String = text.substring(0, start)
          val link: String = text.substring(start+2, end)
          val after: String = text.substring(end+2)
          Chunk(
            if before.isEmpty then None else Some(textMaker(before)),
            Some(resolveWikiLink(link, context, textMaker)),
            if after.isEmpty then None else Some(textMaker(after))
          ).flatten

    loop(text, Chunk.empty)

  private def resolveLink(url: String, context: String): String =
    linkResolver.resolve(
      url,
      category = None,
      context = Some(context),
      anchor = None // TODO
    ) match
      case None => url
      case Some(LinkResolved(url, name)) => url

  private def resolveWikiLink(body: String, context: String, textMaker: String => Inline): Inline =
    val (url: String, text: Option[String]) = Files.split(body.trim, '|')
    linkResolver.resolve(
      url,
      category = None,
      context = Some(context),
      anchor = None // TODO
    ) match
      case None => textMaker(s"[[$body]]")
      case Some(LinkResolved(url, name)) =>
        // Note: in order for the HTML renderer to be able to set correct CSS class on the wiki links,
        // it needs to know which links are wiki links,
        // so I enclose the link text in wiki link brackets...
        Link(
          text = Chunk(textMaker(s"[[${text.getOrElse(name)}]]")),
          url = url,
          title = None
        )
