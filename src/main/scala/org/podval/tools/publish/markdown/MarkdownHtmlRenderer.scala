package org.podval.tools.publish.markdown

import org.podval.tools.publish.Util
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Alignment, Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Doc, Emphasis, HardBreak, Heading, HtmlBlock, HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, SoftBreak, Strikethrough, Strong, Table, TableRow, Text, ThematicBreak}
import zio.blocks.schema.xml.{Xml, XmlBuilder, XmlCodecError, XmlName, XmlReader}

// Note: based on `zio.blocks.docs.HtmlRenderer`.
object MarkdownHtmlRenderer:
  private given CanEqual[Alignment, Alignment] = CanEqual.derived
  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived
  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

  def render(doc: Doc): Either[XmlCodecError, Xml.Element] =
    for children <- renderBlocks(doc.blocks) yield XmlBuilder
      .element("div")
      .children(children*)
      .build

  private def parseHtml(html: String): Either[XmlCodecError, Xml] =
    try Right(XmlReader.read(html))
    catch case e: XmlCodecError => Left(XmlCodecError(e.spans, e.getMessage + ": " + html))

  private def renderBlocks(blocks: Chunk[Block]): Either[XmlCodecError, Chunk[Xml]] =
    Util.sequence(blocks)(renderBlock)

  private def renderInlines(inlines: Chunk[Inline]): Either[XmlCodecError, Chunk[Xml]] =
    Util.sequence(inlines)(renderInline)

  extension (builder: XmlBuilder.ElementBuilder)
    def attr[A](name: String, option: Option[A], value: A => String): XmlBuilder.ElementBuilder =
      option.fold(builder)(a => builder.attr(name, value(a)))

  private def element(name: String, children: Chunk[Xml]): Xml.Element = Xml.Element(name, children*)
  private def element(name: String, child: Xml): Xml.Element = Xml.Element(name, child)

  private def renderBlock(block: Block): Either[XmlCodecError, Xml] = block match
    case Paragraph(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("p").children(children*).build

    case Heading(level, content) =>
      for children <- renderInlines(content) yield XmlBuilder.element(s"h${level.value}").children(children*).build

    case CodeBlock(info, code) =>
      Right(XmlBuilder
        .element("pre")
        .child(
          XmlBuilder
            .element("code")
            .attr("class", info, lang => s"language-$lang")
            .child(XmlBuilder.text(escape(code)))
            .build
        )
        .build
      )

    case ThematicBreak =>
      Right(XmlBuilder.element("hr").build)

    case BlockQuote(content) =>
      for children <- renderBlocks(content) yield XmlBuilder.element("blockquote").children(children*).build

    case BulletList(items, _) =>
      for children <- renderBlocks(items) yield XmlBuilder.element("ul").children(children*).build

    case OrderedList(start, items, _) =>
      for children <- renderBlocks(items) yield XmlBuilder
        .element("ol")
        .attr("start", Option.when(start != 1)(start), _.toString)
        .children(children*)
        .build

    case ListItem(content, checked) =>
      for contentChildren: Chunk[Xml] <- renderBlocks(content) yield element("li", checked match
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
      for
        headerRendered <- renderTableRow(header, alignments, "th")
        rowsRendered <- Util.sequence(rows)(renderTableRow(_, alignments, "td"))
      yield XmlBuilder
        .element("table")
        .child(XmlBuilder
          .element("thead")
          .child(XmlBuilder
            .element("tr")
            .children(headerRendered*)
            .build
          )
          .build
        )
        .child(XmlBuilder
          .element("tbody")
          .children(rowsRendered.map(XmlBuilder
            .element("tr")
            .children(_*)
            .build
          )*)
          .build
        )
        .build

  private def renderTableRow(
    row: TableRow,
    alignments: Chunk[Alignment],
    elementName: String
  ): Either[XmlCodecError, Chunk[Xml.Element]] =
    Util.sequence(row.cells.zip(alignments))((cell, alignment) => for children <- renderInlines(cell) yield XmlBuilder
      .element(elementName)
      .children(children*)
      .attr(
        "style",
        alignment match
          case Alignment.Left   => Some("left")
          case Alignment.Right  => Some("right")
          case Alignment.Center => Some("center")
          case Alignment.None   => None,
        alignment => s"text-align:$alignment"
      )
      .build
    )

  private def renderInline(inline: Inline): Either[XmlCodecError, Xml] = inline match
    case HtmlInline(html) =>
      parseHtml(html)
    case Inline.HtmlInline(html) =>
      parseHtml(html)

    case Text(value) =>
      Right(Xml.Text(value /* TODO? escape(value)*/))
    case Inline.Text(value) =>
      Right(Xml.Text(value /* TODO? escape(value)*/))

    case Code(value) =>
      Right(XmlBuilder.element("code").child(XmlBuilder.text(escape(value))).build)
    case Inline.Code(value) =>
      Right(XmlBuilder.element("code").child(XmlBuilder.text(escape(value))).build)

    case Emphasis(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("em").children(children*).build
    case Inline.Emphasis(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("em").children(children*).build

    case Strong(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("strong").children(children*).build
    case Inline.Strong(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("strong").children(children*).build

    case Strikethrough(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("del").children(children*).build
    case Inline.Strikethrough(content) =>
      for children <- renderInlines(content) yield XmlBuilder.element("del").children(children*).build

    case Link(text, url, titleOpt) =>
      link(text, url, titleOpt)
    case Inline.Link(text, url, titleOpt) =>
      link(text, url, titleOpt)

    case Image(alt, url, titleOpt) =>
      Right(image(alt, url, titleOpt))
    case Inline.Image(alt, url, titleOpt) =>
      Right(image(alt, url, titleOpt))

    case SoftBreak =>
      Right(Xml.Text(" "))
    case Inline.SoftBreak =>
      Right(Xml.Text(" "))

    case HardBreak =>
      Right(XmlBuilder.element("br").build)
    case Inline.HardBreak =>
      Right(XmlBuilder.element("br").build)

    case Autolink(url, isEmail) =>
      Right(autolink(url, isEmail))
    case Inline.Autolink(url, isEmail) =>
      Right(autolink(url, isEmail))

  private def link(
    text: Chunk[Inline],
    url: String,
    titleOpt: Option[String]
  ): Either[XmlCodecError, Xml.Element] = for children <- renderInlines(text) yield XmlBuilder
    .element("a")
    .attr("href", escape(url))
    .attr("title", titleOpt, escape(_))
    .children(children*)
    .build

  private def image(alt: String, url: String, titleOpt: Option[String]): Xml.Element = XmlBuilder
    .element("img")
    .attr("src", escape(url))
    .attr("alt", escape(alt))
    .attr("title", titleOpt, escape(_))
    .build

  private def autolink(url: String, isEmail: Boolean): Xml.Element = XmlBuilder
    .element("a")
    .attr("href", (if isEmail then "mailto:" else "") + escape(url))
    .child(XmlBuilder.text(escape(url)))
    .build

  private def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")
