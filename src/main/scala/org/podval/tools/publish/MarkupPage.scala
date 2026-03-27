package org.podval.tools.publish

import org.podval.tools.publish.html.Html

import java.io.File

object MarkupPage:
  def apply(
    markup: Markup,
    sourceFile: File,
    sourcePath: Path,
    targetPath: Path,
    pageKind: PageKind,
    warnings: Warnings
  ): Either[PageError, Option[MarkupPage]] =
    for
      (frontMatterOrError: Either[PageError, FrontMatter], content: String) = FrontMatter
        .parse(Files.read(sourceFile)) match
        case (Right(frontMatter), content) =>
          (Right(frontMatter), content)
        case (Left(yamlError), content) =>
          (Left(PageError("Malformed front matter", sourcePath, Some(yamlError))), content)
      frontMatter: FrontMatter <- warnings.recover(frontMatterOrError)(FrontMatter.Absent)
      result <- Util.ifDefined(warnings.recoverNone(markup.parse(sourcePath, content))): ast =>
        Right(Some(new MarkupPage(
          sourcePath,
          targetPath,
          pageKind,
          frontMatter,
          markup,
          ast
        )))
    yield
      result

final class MarkupPage private(
  sourcePath: Path,
  targetPath: Path,
  kind: PageKind,
  frontMatter: FrontMatter,
  val markup: Markup,
  astRaw: markup.AST,
) extends Page.WithSource(
  sourcePath,
  targetPath.withExtension(Html.extension),
  kind
):
  def hasFrontMatter: Boolean = !frontMatter.isAbsent

  override def toString: String = s"MarkupPage[$markup]($sourcePath, $targetPath)"

  private var ast: markup.AST = astRaw

  def resolveLinks(links: Links): Unit = ast = markup.resolveLinks(ast, LinksResolver(links, this))
