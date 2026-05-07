package org.podval.tools.publish

import org.podval.tools.publish.PageError
import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.tables.TablesExtension
//import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.data.MutableDataSet

object Markdown extends HtmlLike:
  override val extension: String = "md"
  override val additionalExtensions: Set[String] = Set.empty

  // TODO add missing extensions:
  // - admonitions
  private val flexMarkExtensions: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    //      AnchorLinkExtension.create,
    //      AutolinkExtension.create,
    TablesExtension.create,
    //      TocExtension.create,
    //      WikiLinkExtension.create,
    //      StrikethroughExtension.create
  )

  private val options: MutableDataSet = new MutableDataSet
//  options.set(WikiLinkExtension.ALLOW_ANCHORS, true)
//  options.set(WikiLinkExtension.DISABLE_RENDERING, true)

//    options.set(Parser.HTML_BLOCK_DEEP_PARSER, true)
//    options.set(Parser.HTML_BLOCK_DEEP_PARSE_NON_BLOCK, true)

  private val parser: Parser = Parser
    .builder(options)
    .extensions(flexMarkExtensions.asJava)
    .build

  private val renderer: HtmlRenderer = HtmlRenderer
    .builder(options)
    .extensions(flexMarkExtensions.asJava)
    .build

  override def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element] =
    // TODO catch errors!
    val doc = parser.parse(content)
    // Wrap Markdown rendered as HTML in a 'div' and parse.
    HtmlLike.Html.parse(sourcePath, s"<div>${renderer.render(doc)}</div>")

