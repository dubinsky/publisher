package org.podval.tools.publish.markdown

import com.vladsch.flexmark.ast.Heading
import com.vladsch.flexmark.ext.toc.TocExtension
import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
import com.vladsch.flexmark.html.HtmlRenderer
import com.vladsch.flexmark.parser.Parser
import com.vladsch.flexmark.util.ast.{Document, Node}
import com.vladsch.flexmark.util.data.{DataHolder, MutableDataSet}
import org.podval.tools.publish.Markup
import scala.jdk.CollectionConverters.SeqHasAsJava

object Markdown extends Markup(
  extension = "md",
  additionalExtensions = Set.empty
):
  override type AST = Document

  override def read(content: String): AST = parser.parse(content)

  def extractTitle(node: Node): (Option[String], Node) =
    val heading: Option[Heading] =
      val firstChild: Node = node.getFirstChild
      if !firstChild.isInstanceOf[Heading] then None else
        val result: Heading = firstChild.asInstanceOf[Heading]
        if result.getLevel != 1 then None else Some(result)

    // Note: heading is removed from the ast in-place!
    heading.foreach(_.unlink)

    val title: Option[String] = heading.map(_.getText.toString)

    (title, node)

  private lazy val options: DataHolder =
    val result: MutableDataSet = new MutableDataSet

    // uncomment to set optional extensions
    //result.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create, StrikethroughExtension.create));

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

  private lazy val renderer: HtmlRenderer = HtmlRenderer
    .builder(options)
    .extensions(Seq(
      TocExtension.create,
      WikiLinkExtension.create
    ).asJava)
    .build

  def main(args: Array[String]): Unit =
    val doc: Document = parser.parse(
      """# TITLE
        |blah [[page|test]] asff
        |""".stripMargin
    )
    val x = 0