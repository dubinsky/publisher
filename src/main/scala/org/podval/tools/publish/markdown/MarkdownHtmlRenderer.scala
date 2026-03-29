package org.podval.tools.publish.markdown

import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Alignment, Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Doc, Emphasis, HardBreak,
  Heading, HtmlBlock, HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, SoftBreak, Strikethrough,
  Strong, Table, TableRow, Text, ThematicBreak}
import zio.blocks.schema.xml.{Xml, XmlName, XmlReader}

// Note: based on `zio.blocks.docs.HtmlRenderer`.
object MarkdownHtmlRenderer:
  private given CanEqual[Alignment, Alignment] = CanEqual.derived
  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived
  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

  private def element(name: String, children: Chunk[Xml]): Xml.Element = Xml.Element(name, children*)
  private def element(name: String, child: Xml): Xml.Element = Xml.Element(name, child)
  private def element(name: String): Xml.Element = Xml.Element(name)

  def render(doc: Doc): Chunk[Xml] =
    renderBlocks(doc.blocks)

  private def renderBlocks(blocks: Chunk[Block]): Chunk[Xml] =
    blocks.map(renderBlock)

  private def renderBlock(block: Block): Xml = block match
    case Paragraph(content) =>
      element("p", renderInlines(content))

    case Heading(level, content) =>
      element(s"h${level.value}", renderInlines(content))

    case CodeBlock(info, code) =>
      element("pre",
        Xml.Element(
          name = XmlName("code"),
          children = Chunk(Xml.Text(escape(code))),
          attributes = Chunk.empty ++ info.map(
            lang => XmlName("class") -> s"language-$lang"
          )
        )
      )

    case ThematicBreak =>
      element("hr")

    case BlockQuote(content) =>
      element("blockquote", renderBlocks(content))

    case BulletList(items, _) =>
      element("ul", renderBlocks(items))

    case OrderedList(start, items, _) =>
      Xml.Element(
        name = XmlName("ol"),
        children = renderBlocks(items),
        attributes = Chunk.empty ++ Option.when(start != 1)(
          XmlName("start") -> s"$start"
        )
      )

    case ListItem(content, checked) =>
      val contentChildren: Chunk[Xml] = renderBlocks(content)
      element("li", checked match
        case None => contentChildren
        case Some(checked)  => Chunk(Xml.Element(
          name = XmlName("input"),
          children = contentChildren,
          attributes = Chunk(
            XmlName("type") -> "checkbox",
            XmlName("disabled") -> "" // TODO or "disabled"?
          ) ++ Option.when(checked)(
            XmlName("checked") -> "" // TODO or "checked"?
          )
        ))
      )

    case HtmlBlock(html) =>
      parseHtml(html)

    case Table(header, alignments, rows) =>
      Xml.Element("table",
        element("thead",
          element("tr", renderTableHeader(header, alignments))
        ),
        element("tbody",
          rows.map(row => element("tr", renderTableRow(row, alignments)))
        )
      )

  private def element(name: String, alignment: Alignment, children: Chunk[Xml]): Xml.Element =
    Xml.Element(
      name = XmlName(name),
      children = children,
      attributes = alignment match
        case Alignment.Left => Some("left")
        case Alignment.Right => Some("right")
        case Alignment.Center => Some("center")
        case Alignment.None => None
      match
        case None => Chunk.empty
        case Some(alignment) => Chunk(XmlName("style") -> s"text-align:$alignment")
    )

  private def renderTableHeader(row: TableRow, alignments: Chunk[Alignment]): Chunk[Xml.Element] = row.cells
    .zip(alignments)
    .map((cell, alignment) => element("th", alignment, renderInlines(cell)))

  private def renderTableRow(row: TableRow, alignments: Chunk[Alignment]): Chunk[Xml.Element] = row.cells
    .zip(alignments)
    .map((cell, alignment) => element("td", alignment, renderInlines(cell)))

  private def renderInlines(inlines: Chunk[Inline]): Chunk[Xml] =
    inlines.map(renderInline)

  private def renderInline(inline: Inline): Xml = inline match
    case Text(value) =>
      Xml.Text(escape(value))
    case Inline.Text(value) =>
      Xml.Text(escape(value))

    case Code(value) =>
      element("code", Xml.Text(escape(value)))
    case Inline.Code(value) =>
      element("code", Xml.Text(escape(value)))

    case Emphasis(content) =>
      element("em", renderInlines(content))
    case Inline.Emphasis(content) =>
      element("em", renderInlines(content))

    case Strong(content) =>
      element("strong", renderInlines(content))
    case Inline.Strong(content) =>
      element("strong", renderInlines(content))

    case Strikethrough(content) =>
      element("del", renderInlines(content))
    case Inline.Strikethrough(content) =>
      element("del", renderInlines(content))

    case Link(text, url, titleOpt) =>
      link(text, url, titleOpt)
    case Inline.Link(text, url, titleOpt) =>
      link(text, url, titleOpt)

    case Image(alt, url, titleOpt) =>
      image(alt, url, titleOpt)
    case Inline.Image(alt, url, titleOpt) =>
      image(alt, url, titleOpt)

    case HtmlInline(html) =>
      parseHtml(html)
    case Inline.HtmlInline(html) =>
      parseHtml(html)

    case SoftBreak =>
      Xml.Text(" ")
    case Inline.SoftBreak =>
      Xml.Text(" ")

    case HardBreak =>
      element("<br>")
    case Inline.HardBreak =>
      element("<br>")

    case Autolink(url, isEmail) =>
      autolink(url, isEmail)
    case Inline.Autolink(url, isEmail) =>
      autolink(url, isEmail)

  private def link(text: Chunk[Inline], url: String, titleOpt: Option[String]): Xml.Element = Xml.Element(
    name = XmlName("a"),
    children = renderInlines(text),
    attributes = Chunk(
      XmlName("href") -> escape(url)
    ) ++ titleOpt.map(title =>
      XmlName("title") -> escape(title)
    )
  )

  private def image(alt: String, url: String, titleOpt: Option[String]): Xml.Element = Xml.Element(
    name = XmlName("img"),
    children = Chunk.empty,
    attributes = Chunk(
      XmlName("src") -> escape(url),
      XmlName("alt") -> escape(alt)
    ) ++ titleOpt.map(title =>
      XmlName("title") -> escape(title)
    )
  )

  private def autolink(url: String, isEmail: Boolean): Xml.Element =
    Xml.Element(
      name = XmlName("a"),
      children = Chunk(Xml.Text(escape(url))),
      attributes = Chunk(XmlName("href") -> ((if isEmail then "mailto:" else "") + escape(url)))
    )

  private def parseHtml(html: String): Xml =
    XmlReader.read(html).toOption.get

  private def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&#39;")
