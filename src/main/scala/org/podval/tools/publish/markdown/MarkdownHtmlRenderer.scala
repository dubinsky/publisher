package org.podval.tools.publish.markdown

import org.podval.tools.publish.Util
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Alignment, Autolink, Block, BlockQuote, BulletList, Code, CodeBlock, Doc, Emphasis, HardBreak,
  Heading, HtmlBlock, HtmlInline, Image, Inline, Link, ListItem, OrderedList, Paragraph, SoftBreak, Strikethrough,
  Strong, Table, TableRow, Text, ThematicBreak}
import zio.blocks.schema.xml.{Xml, XmlCodecError, XmlName, XmlReader}

// Note: based on `zio.blocks.docs.HtmlRenderer`.
object MarkdownHtmlRenderer:
  private given CanEqual[Alignment, Alignment] = CanEqual.derived
  private given CanEqual[ThematicBreak.type, Block] = CanEqual.derived
  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

  def render(doc: Doc): Either[XmlCodecError, Xml.Element] = renderBlocks(
    "div",
    doc.blocks
  )

  private def parseHtml(html: String): Either[XmlCodecError, Xml] =
    try Right(XmlReader.read(html))
    catch case e: XmlCodecError => Left(XmlCodecError(e.spans, e.getMessage + ": " + html))

  private def renderBlocks(
    elementName: String,
    blocks: Chunk[Block],
    attributes: Chunk[(XmlName, String)] = Chunk.empty
  ): Either[XmlCodecError, Xml.Element] =
    for children: Chunk[Xml] <- Util.sequence(blocks)(renderBlock)
    yield element(elementName, children, attributes)

  private def renderInlines(
    elementName: String,
    inlines: Chunk[Inline],
    attributes: Chunk[(XmlName, String)] = Chunk.empty
  ): Either[XmlCodecError, Xml.Element] =
    for children: Chunk[Xml] <- Util.sequence(inlines)(renderInline)
    yield element(elementName, children, attributes)

  private def element(name: String, children: Chunk[Xml], attributes: Chunk[(XmlName, String)]): Xml.Element =
    Xml.Element(name = XmlName(name), children = children, attributes = attributes)

  private def element(name: String, children: Chunk[Xml]): Xml.Element = Xml.Element(name, children*)
  private def element(name: String, child: Xml): Xml.Element = Xml.Element(name, child)
  private def element(name: String): Xml.Element = Xml.Element(name)

  private def renderBlock(block: Block): Either[XmlCodecError, Xml] = block match
    case Paragraph(content) =>
      renderInlines("p", content)

    case Heading(level, content) =>
      renderInlines(s"h${level.value}", content)

    case CodeBlock(info, code) =>
      Right(element("pre", element("code", Chunk(Xml.Text(escape(code))),
        attributes = Chunk.empty ++ info.map(
          lang => XmlName("class") -> s"language-$lang"
        )
      )))

    case ThematicBreak =>
      Right(element("hr"))

    case BlockQuote(content) =>
      renderBlocks("blockquote", content)

    case BulletList(items, _) =>
      renderBlocks("ul", items)

    case OrderedList(start, items, _) =>
      renderBlocks("ol", items,
        attributes = Chunk.empty ++ Option.when(start != 1)(
          XmlName("start") -> s"$start"
        )
      )

    case ListItem(content, checked) =>
      for contentChildren: Chunk[Xml] <- Util.sequence(content)(renderBlock) yield element("li", checked match
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
      yield Xml.Element("table",
        element("thead", element("tr", headerRendered)),
        element("tbody", rowsRendered.map(element("tr", _)))
      )

  private def renderTableRow(
    row: TableRow,
    alignments: Chunk[Alignment],
    elementName: String
  ): Either[XmlCodecError, Chunk[Xml.Element]] =
    Util.sequence(row.cells.zip(alignments))((cell, alignment) => renderInlines(
      elementName = elementName,
      inlines = cell,
      attributes = alignment match
        case Alignment.Left => Some("left")
        case Alignment.Right => Some("right")
        case Alignment.Center => Some("center")
        case Alignment.None => None
      match
        case None => Chunk.empty
        case Some(alignment) => Chunk(XmlName("style") -> s"text-align:$alignment")
    ))

  private def renderInline(inline: Inline): Either[XmlCodecError, Xml] = inline match
    case HtmlInline(html) =>
      parseHtml(html)
    case Inline.HtmlInline(html) =>
      parseHtml(html)

    case Text(value) =>
      Right(Xml.Text(escape(value)))
    case Inline.Text(value) =>
      Right(Xml.Text(escape(value)))

    case Code(value) =>
      Right(element("code", Xml.Text(escape(value))))
    case Inline.Code(value) =>
      Right(element("code", Xml.Text(escape(value))))

    case Emphasis(content) =>
      renderInlines("em", content)
    case Inline.Emphasis(content) =>
      renderInlines("em", content)

    case Strong(content) =>
      renderInlines("strong", content)
    case Inline.Strong(content) =>
      renderInlines("strong", content)

    case Strikethrough(content) =>
      renderInlines("del", content)
    case Inline.Strikethrough(content) =>
      renderInlines("del", content)

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
      Right(element("br"))
    case Inline.HardBreak =>
      Right(element("br"))

    case Autolink(url, isEmail) =>
      Right(autolink(url, isEmail))
    case Inline.Autolink(url, isEmail) =>
      Right(autolink(url, isEmail))

  private def link(
    text: Chunk[Inline],
    url: String,
    titleOpt: Option[String]
  ): Either[XmlCodecError, Xml.Element] = renderInlines(
    elementName = "a",
    inlines = text,
    attributes = Chunk(
      XmlName("href") -> escape(url)
    ) ++ titleOpt.map(title =>
      XmlName("title") -> escape(title)
    )
  )

  private def image(alt: String, url: String, titleOpt: Option[String]): Xml.Element = element(
    name = "img",
    children = Chunk.empty,
    attributes = Chunk(
      XmlName("src") -> escape(url),
      XmlName("alt") -> escape(alt)
    ) ++ titleOpt.map(title =>
      XmlName("title") -> escape(title)
    )
  )

  private def autolink(url: String, isEmail: Boolean): Xml.Element = element(
    name = "a",
    children = Chunk(Xml.Text(escape(url))),
    attributes = Chunk(XmlName("href") -> ((if isEmail then "mailto:" else "") + escape(url)))
  )

  private def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&#39;")
