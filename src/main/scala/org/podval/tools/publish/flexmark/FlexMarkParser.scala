package org.podval.tools.publish.flexmark

// TODO right a note about the approach chosen!
import org.podval.tools.publish.PageError

import scala.jdk.CollectionConverters.SeqHasAsJava
//import com.vladsch.flexmark.ast.Heading
import com.vladsch.flexmark.ext.anchorlink.AnchorLinkExtension
import com.vladsch.flexmark.ext.autolink.AutolinkExtension
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
//import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.{Document, Node}
import com.vladsch.flexmark.util.data.{DataHolder, MutableDataSet}

object FlexMarkParser:
//  override type AST = Document
//
//  override def read(sourcePath: Path, content: String): Either[PageError, AST] = Right(parser.parse(content))
//
//  def extractTitle(node: Node): (Option[String], Node) =
//    val heading: Option[Heading] =
//      val firstChild: Node = node.getFirstChild
//      if !firstChild.isInstanceOf[Heading] then None else
//        val result: Heading = firstChild.asInstanceOf[Heading]
//        if result.getLevel != 1 then None else Some(result)
//
//    // Note: heading is removed from the ast in-place!
//    heading.foreach(_.unlink)
//
//    val title: Option[String] = heading.map(_.getText.toString)
//
//    (title, node)
//
  private lazy val options: DataHolder =
    val result: MutableDataSet = new MutableDataSet

    result.set(Parser.HTML_BLOCK_DEEP_PARSER, true)
    result.set(Parser.HTML_BLOCK_DEEP_PARSE_NON_BLOCK, true)

    result.set(Parser.EXTENSIONS, List(
      AnchorLinkExtension.create,
      AutolinkExtension.create,
      TocExtension.create,
      WikiLinkExtension.create,
//      TablesExtension.create,
//      StrikethroughExtension.create
    ).asJava)

    // uncomment to convert soft-breaks to hard breaks
    //result.set(HtmlRenderer.SOFT_BREAK, "<br />\n");

    result

  // You can re-use parser and renderer instances
  private lazy val parser: Parser = Parser
    .builder(options)
    .extensions(Seq(
      TocExtension.create,
      WikiLinkExtension.create
    ).asJava)
    .build

//  def parse(content: String): Either[PageError, Document] = Right(parser.parse(content))
  def parse(content: String): Document = parser.parse(content)

  def main(args: Array[String]): Unit =
    val document = parse(
      """# Title
        |
        |<a href="#a">text</a>
        |""".stripMargin
    )
    val x = 0

//  private lazy val renderer: HtmlRenderer = HtmlRenderer
//    .builder(options)
//    .extensions(Seq(
//      TocExtension.create,
//      WikiLinkExtension.create
//    ).asJava)
//    .build
