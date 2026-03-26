package org.podval.tools.publish.markdown

import org.podval.tools.publish.{LinkResolver, Markup, MarkupPage, PageError, Path}
import zio.blocks.docs.{Doc, Parser}
// TODO right a note about the approach chosen!
//import scala.jdk.CollectionConverters.SeqHasAsJava
//import com.vladsch.flexmark.ast.Heading
//import com.vladsch.flexmark.ext.toc.TocExtension
//import com.vladsch.flexmark.ext.wikilink.WikiLinkExtension
//import com.vladsch.flexmark.html.HtmlRenderer
//import com.vladsch.flexmark.parser.Parser
//import com.vladsch.flexmark.util.ast.{Document, Node}
//import com.vladsch.flexmark.util.data.{DataHolder, MutableDataSet}

object Markdown extends Markup(
  extension = "md",
  additionalExtensions = Set.empty
):
  val test: String =
    """# TITLE [[tp]]
      |blah [[page|test]] *asd*
      |   asff
      |""".stripMargin

//  def main(args: Array[String]): Unit = parse(Path.root, test) match
//    case Right(ast) =>
////      println(ast)
////      println(s"-----")
//      new ResolveLinks().resolveDoc(ast.normalize)
//    case Left(error) => println(error)

  override type AST = Doc

  override def parse(sourcePath: Path, content: String): Either[PageError, Doc] =
    Parser.parse(content) match
      case Right(doc) => Right(doc)
      case Left(error) => Left(PageError(s"Markdown parse error: $error", sourcePath))

  override def resolveLinks(doc: Doc, linkResolver: LinkResolver): Unit = ResolveLinks(linkResolver).resolveDoc(doc)

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
//  private lazy val options: DataHolder =
//    val result: MutableDataSet = new MutableDataSet
//
//    // uncomment to set optional extensions
//    //result.set(Parser.EXTENSIONS, Arrays.asList(TablesExtension.create, StrikethroughExtension.create));
//
//    // uncomment to convert soft-breaks to hard breaks
//    //result.set(HtmlRenderer.SOFT_BREAK, "<br />\n");
//
//    result
//
//  // You can re-use parser and renderer instances
//  private lazy val parser: Parser = Parser
//    .builder(options)
//    .extensions(Seq(
//      TocExtension.create,
//      WikiLinkExtension.create
//    ).asJava)
//    .build
//
//  private lazy val renderer: HtmlRenderer = HtmlRenderer
//    .builder(options)
//    .extensions(Seq(
//      TocExtension.create,
//      WikiLinkExtension.create
//    ).asJava)
//    .build
