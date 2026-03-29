package org.podval.tools.publish

import org.podval.tools.publish.html.Html

import java.io.File
import Util.ifDefined
import zio.blocks.schema.xml.Xml

sealed trait Page:
  def sourcePath: Path
  def targetPath: Path
  def pageKind: PageKind

  final def path: Path = targetPath.withoutExtension

object Page:
  final class Asset(
    override val sourcePath: Path,
    override val targetPath: Path,
    override val pageKind: PageKind
  ) extends Page:
    override def toString: String = s"Asset($sourcePath, $targetPath)"

  final class SyntheticAsset(
    override val targetPath: Path,
    override val pageKind: PageKind,
    val content: String
  ) extends Page:
    override def toString: String = s"SyntheticAsset($targetPath)"
    override def sourcePath: Path = targetPath

  // TODO extract Anchors at creation
  final class MarkupPage(
    override val sourcePath: Path,
    override val targetPath: Path,
    override val pageKind: PageKind,
    val markup: Markup,
    frontMatter: FrontMatter,
    astRaw: markup.AST,
  ) extends Page:
    override def toString: String = s"Markup[$markup]($sourcePath, $targetPath)"

    private var ast: markup.AST = astRaw

    def resolveLinks(links: Links): Unit = ast = markup.resolveLinks(ast, LinksResolver(links, this))

    def render: Xml = markup.render(ast) // TODO add TOC


  def makePage(
    sourcePath: Path,
    sourceFile: File,
    pageKind: PageKind,
    warnings: Warnings
  ): Either[PageError, Option[Page]] =
    val markup: Option[Markup] = sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension)))

    ifDefined(warnings.recoverNone(
      if pageKind.isMarkupAllowed(markup)
      then Right(())
      else Left(PageError(s"Markup $markup not allowed in $pageKind", sourcePath))
    )): _ =>
      ifDefined(warnings.recoverNone(pageKind.targetPath(sourcePath))): (targetPath: Path) =>
        ifDefined(markup match
          case None => Right(Some(Page.Asset(
            sourcePath = sourcePath,
            targetPath = targetPath,
            pageKind = pageKind
          )))
          case Some(markup) => makeMarkupPage(
            markup = markup,
            sourceFile = sourceFile,
            sourcePath = sourcePath,
            targetPath = targetPath.withExtension(Html.extension),
            pageKind = pageKind,
            warnings = warnings
          )
        ): (page: Page) =>
          Right(Some(page))

  private def makeMarkupPage(
    markup: Markup,
    sourceFile: File,
    sourcePath: Path,
    targetPath: Path,
    pageKind: PageKind,
    warnings: Warnings
  ): Either[PageError, Option[Page.MarkupPage]] =
    for
      (frontMatterOrError: Either[PageError, FrontMatter], content: String) = FrontMatter
        .parse(Files.read(sourceFile)) match
        case (Right(frontMatter), content) =>
          (Right(frontMatter), content)
        case (Left(yamlError), content) =>
          (Left(PageError("Malformed front matter", sourcePath, Some(yamlError))), content)
      // treat markup files without front matter as assets!!!
      frontMatter: FrontMatter <- warnings.recover(frontMatterOrError)(FrontMatter.Absent)
      result <- ifDefined(warnings.recoverNone(markup.parse(sourcePath, content))): ast =>
        Right(Some(new Page.MarkupPage(
          sourcePath,
          targetPath,
          pageKind,
          markup,
          frontMatter,
          ast
        )))
    yield
      result
