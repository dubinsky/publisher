package org.podval.tools.publish.markdown

import org.podval.tools.publish.{LinkResolver, MarkupPage}
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Doc, Emphasis, HardBreak, Heading,
  HtmlBlock, HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, Renderer, SoftBreak, Strikethrough,
  Strong, Table, TableRow, Text, ThematicBreak}
import scala.annotation.tailrec

final class ResolveLinks(linkResolver: LinkResolver):
  def resolveDoc(doc: Doc): Doc = Doc(metadata = doc.metadata, blocks = resolveBlocks(doc.blocks))

  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived
  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

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
    case HtmlBlock(content) => ???
    case Table(header, alignments, rows) => Table(resolveTableRow(header), alignments, rows.map(resolveTableRow))

  private def resolveListItems(items: Chunk[ListItem]): Chunk[ListItem] = items.map(resolveListItem)
  private def resolveListItem(item: ListItem): ListItem = ListItem(resolveBlocks(item.content), item.checked)
  private def resolveTableRow(row: TableRow): TableRow = TableRow(row.cells.map(resolveInlines))

  private def resolveInlines(inlines: Chunk[Inline]): Chunk[Inline] =
    splitIntoLines(inlines).flatMap: line =>
      val context: String = Renderer.renderInlines(line).trim // TODO render to HTML string?
      line.flatMap:
        case Text(value) => resolveText(value, context, Text.apply)
        case Inline.Text(value) => resolveText(value, context, Inline.Text.apply)
        case inline: Inline => Chunk:
          inline match
            case Link(text, url, title) => Link(text, url, title) // TODO resolve URL
            case Inline.Link(text, url, title) => Inline.Link(text, url, title)
            case Image(alt, url, title) => Image(alt, url, title) // TODO resolve URL
            case Inline.Image(alt, url, title) => Inline.Image(alt, url, title)
            case Code(value) => Code(value)
            case Inline.Code(value) => Inline.Code(value)
            case Emphasis(content) => Emphasis(resolveInlines(content))
            case Inline.Emphasis(content) => Inline.Emphasis(resolveInlines(content))
            case Strong(content) => Strong(resolveInlines(content))
            case Inline.Strong(content) => Inline.Strong(resolveInlines(content))
            case Strikethrough(content) => Strikethrough(resolveInlines(content))
            case Inline.Strikethrough(content) => Inline.Strikethrough(resolveInlines(content))
            case HtmlInline(content) => HtmlInline(content)
            case Inline.HtmlInline(content) => Inline.HtmlInline(content)
            case SoftBreak => SoftBreak
            case Inline.SoftBreak => Inline.SoftBreak
            case HardBreak => HardBreak
            case Inline.HardBreak => Inline.HardBreak
            case Autolink(url, isEmail) => Autolink(url, isEmail)
            case Inline.Autolink(url, isEmail) => Inline.Autolink(url, isEmail)
            case _ => throw IllegalStateException("Can not happen!")

  private def resolveText(
    value: String,
    context: String,
    textMaker: String => Inline
  ): Chunk[Inline] =
    println(s"Resolving: $value in $context")
    // TODO resolve wiki links
    Chunk(textMaker(value))

  private def splitIntoLines(inlines: Chunk[Inline]): Chunk[Chunk[Inline]] =
    @tailrec
    def loop(inlines: Chunk[Inline], result: Chunk[Chunk[Inline]]): Chunk[Chunk[Inline]] =
      if inlines.isEmpty then result else
        val (start: Chunk[Inline], rest: Chunk[Inline]) = inlines.splitWhere(isBreak)
        val (line: Chunk[Inline], tail: Chunk[Inline]) =
          if rest.isEmpty
          then (start, rest)
          else (start :+ rest.head, rest.tail)
        loop(tail, result.appended(line))

    loop(inlines, Chunk.empty)

  private def isBreak(inline: Inline): Boolean = inline match
    case SoftBreak => true
    case Inline.SoftBreak => true
    case HardBreak => true
    case Inline.HardBreak => true
    case _ => false
