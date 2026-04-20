package org.podval.tools.publish

import org.podval.tools.publish.PageError
import zio.blocks.schema.xml.Xml

import scala.jdk.CollectionConverters.SeqHasAsJava
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.data.MutableDataSet
import Html.{childWhen, el}

import scala.annotation.tailrec

object MarkdownFlexMark extends Markup(
  extension = "md",
  additionalExtensions = Set.empty
):
  override def linkElementResolvers: Seq[LinkElementResolver] = Seq(
    LinkElementResolver.A
  )

  private val flexMarkExtensions: List[Parser.ParserExtension & HtmlRenderer.HtmlRendererExtension] = List(
    //      AnchorLinkExtension.create,
    //      AutolinkExtension.create,
    //      TocExtension.create,
    //      WikiLinkExtension.create,
    //      TablesExtension.create,
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

  def parse(sourcePath: Path, content: String): Either[PageError, Xml] =
    // TODO catch errors!
    val doc = parser.parse(content)
    Html
      .parseDiv(sourcePath, renderer.render(doc))
      .map(processWikiLinks)

  // Processes wiki links.
  private def processWikiLinks(element: Xml.Element): Xml =
    def renderWikiLink(body: String): Xml =
      val (url: String, text: Option[String]) = Files.split(body.trim, '|')
      val textTrimmed = text.fold("")(_.trim)
      el("a", "class" -> "wiki-link", "href" -> url.trim)
        .childWhen(textTrimmed.nonEmpty, Xml.Text(textTrimmed))
        .build

    @tailrec
    def loop(result: Seq[Xml], text: String): Seq[Xml] =
      if text.isEmpty then result else
        val start: Int = text.indexOf("[[")
        val end: Int = if start == -1 then -1 else text.indexOf("]]", start + 2)
        if end == -1
        then loop(result ++ Seq(Xml.Text(text)), "")
        else
          val before: String = text.substring(0, start)
          loop(
            result = result ++
              Option.when(before.nonEmpty)(Xml.Text(before)).toSeq ++
              Seq(renderWikiLink(text.substring(start + 2, end))),
            text = text.substring(end + 2)
          )

    Xml.Element(element.name, element.attributes, element.children.flatMap {
      case element: Xml.Element => Seq(processWikiLinks(element))
      case Xml.Text(value) => loop(Seq.empty, value)
      case xml => Seq(xml)
    })

  // WOW! ZIO Blocks Markdown parser misparses nested lists!
  // This is what gets printed:
  //<div>
  //  <h1>Nested Lists</h1>
  //  <ul>
  //    <li>
  //      <p>First item</p>
  //    </li>
  //    <li>
  //      <p>Second item</p>
  //    </li>
  //    <li>
  //      <p>Third item</p>
  //    </li>
  //    <li>
  //      <p>Indented item</p>
  //    </li>
  //    <li>
  //      <p>Indented item</p>
  //    </li>
  //    <li>
  //      <p>Fourth item</p>
  //    </li>
  //  </ul>
  //</div>
  //
  //  And this is what gets printed when using FlexMark:
  //<div>
  //  <h1>Nested Lists</h1>
  //  <ul>
  //    <li>First item</li>
  //    <li>Second item</li>
  //    <li>
  //      Third item
  //      <ul>
  //        <li>Indented item</li>
  //        <li>Indented item</li>
  //      </ul>
  //    </li>
  //    <li>Fourth item</li>
  //  </ul>
  //</div>
  //
  // FlexMark WikiLink extension is useless to me:
  // - it sets the content of the rendered HTML 'a' element when there is no | in the link;
  // - it prefixes the href with the dashes corresponding in number to the spaces after the |...
  def main(args: Array[String]): Unit =
    val xml = parse(Path.root,
      """# Nested Lists
        |
        |  [[name]]  asdada [[name#anchor]]
        |
        |  [[name| text]]
        |""".stripMargin
    ).toOption.get
    println(Html.write(xml))
