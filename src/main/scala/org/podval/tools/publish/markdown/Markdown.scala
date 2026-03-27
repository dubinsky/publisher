package org.podval.tools.publish.markdown

import org.podval.tools.publish.{LinksResolver, Markup, PageError, Path}
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Doc, HardBreak, Inline, Parser, SoftBreak}
import scala.annotation.tailrec

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
  override type AST = Doc

  override def parse(sourcePath: Path, content: String): Either[PageError, Doc] =
    Parser.parse(content) match
      case Right(doc) => Right(doc)
      case Left(error) => Left(PageError(s"Markdown parse error: $error", sourcePath))

  override def resolveLinks(doc: Doc, linkResolver: LinksResolver): Doc =
    MarkdownLinksResolver(linkResolver).resolveDoc(doc)

  def splitIntoLines(inlines: Chunk[Inline]): Chunk[Chunk[Inline]] =
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

  private given s: CanEqual[SoftBreak.type, Inline] = CanEqual.derived
  private given h: CanEqual[HardBreak.type, Inline] = CanEqual.derived
  private given si: CanEqual[Inline.SoftBreak.type, Inline] = CanEqual.derived
  private given hi: CanEqual[Inline.HardBreak.type, Inline] = CanEqual.derived

  def isBreak(inline: Inline): Boolean = inline match
    case SoftBreak => true
    case Inline.SoftBreak => true
    case HardBreak => true
    case Inline.HardBreak => true
    case _ => false

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
