package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.io.File
import Util.ifDefined

// TODO add embedded asset pages and use them for CSS

sealed trait Page derives CanEqual:
  def sourcePath: Path
  def targetPath: Path
  
  final def path: Path = targetPath.withoutExtension

  override def equals(obj: Any): Boolean = obj match
    case that: Page => this.targetPath == that.targetPath
    case _ => false

object Page:
  final class Asset(
    override val sourcePath: Path,
    override val targetPath: Path
  ) extends Page:
    override def toString: String = s"Asset($sourcePath, $targetPath)"

  // TODO load from resource
  final class SyntheticAsset(
    override val targetPath: Path,
    val content: String
  ) extends Page:
    override def toString: String = s"SyntheticAsset($targetPath)"
    override def sourcePath: Path = targetPath

  // TODO extract Anchors at creation
  final class MarkupPage(
    override val sourcePath: Path,
    override val targetPath: Path,
    val pageKind: PageKind,
    val markup: Markup,
    val frontMatter: FrontMatter,
    xmlRaw: Xml,
  ) extends Page:
    override def toString: String = s"Markup[$markup]($sourcePath, $targetPath)"

    private var xml: Xml = xmlRaw
    
    def getXml: Xml = xml

    def resolveLinks(links: Links): Unit = xml = links.resolve(xml, this)
    
    def title: String = frontMatter.title.getOrElse(targetPath.path.last) // TODO titl from the document itself?

  def makePage(
    sourcePath: Path,
    sourceFile: File,
    pageKind: PageKind,
    warnings: Warnings
  ): Either[PageError, Option[Page]] =
    val markup: Option[Markup] = sourcePath.extension.flatMap(extension => Markup.all.find(_.isExtension(extension)))

    ifDefined(warnings.recoverNone(pageKind.targetPath(sourcePath))): (targetPath: Path) =>
      def makeAsset: Either[PageError, Option[Asset]] = ifDefined(warnings.recoverNone(
        if pageKind.isAssetAllowed
        then Right(())
        else Left(PageError(sourcePath, s"Asset not allowed in $pageKind"))
      )): _ =>
        Right(Some(Page.Asset(
          sourcePath = sourcePath,
          targetPath = targetPath
        )))

      ifDefined(markup match
        case None => makeAsset
        case Some(markup) =>
          ifDefined(warnings.recoverNone(
            if pageKind.isMarkupAllowed(markup)
            then Right(())
            else Left(PageError(sourcePath, s"Markup $markup not allowed in $pageKind"))
          )): _ =>
            val (frontMatterOrError: Either[PageError, FrontMatter], content: String) = FrontMatter
              .parse(Files.read(sourceFile)) match
                case (Right(frontMatter), content) =>
                  (Right(frontMatter), content)
                case (Left(yamlError), content) =>
                  (Left(PageError(sourcePath, "Malformed FrontMatter", Some(yamlError))), content)

            for
              frontMatter: FrontMatter <- warnings.recover(frontMatterOrError)(FrontMatter.absent)
              result: Option[Page] <- if frontMatter.isAbsent then makeAsset else
                ifDefined(warnings.recoverNone(markup.parse(sourcePath, content))): xml =>
                  Right(Some(Page.MarkupPage(
                    sourcePath,
                    targetPath.withExtension(Html.extension),
                    pageKind,
                    markup,
                    frontMatter,
                    xml
                  )))
            yield
              result
      ): (page: Page) =>
        Right(Some(page))

