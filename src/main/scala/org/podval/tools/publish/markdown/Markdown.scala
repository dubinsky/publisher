package org.podval.tools.publish.markdown

import org.podval.tools.publish.{LinksResolver, Markup, PageError, Path}
import zio.blocks.chunk.Chunk
import zio.blocks.docs.{Doc, HardBreak, Inline, Parser, SoftBreak}
import zio.blocks.schema.xml.Xml
import scala.annotation.tailrec

object Markdown extends Markup(
  extension = "md",
  additionalExtensions = Set.empty
):
  override type AST = Doc

  override def parse(sourcePath: Path, content: String): Either[PageError, Doc] = Parser.parse(content) match
    case Right(doc) => Right(doc)
    case Left(error) => Left(PageError(sourcePath, s"Malformed Markdown: $error"))

  override def reformat(doc: Doc): Option[String] = None // TODO pretty-print

  override def resolveLinks(doc: Doc, linkResolver: LinksResolver): Doc =
    MarkdownLinksResolver(linkResolver).resolveDoc(doc)

  override def render(doc: Doc): Xml =
    Xml.Element("div", MarkdownHtmlRenderer.render(doc)*)

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
