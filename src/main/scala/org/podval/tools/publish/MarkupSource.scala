package org.podval.tools.publish

import org.podval.tools.publish.util.Files
import org.podval.xml.Xml
import scala.ref.SoftReference

final class MarkupSource(
  val site: Site,
  val markup: Markup,
  val sourcePath: Path
):
  private var cachedVar: Option[SoftReference[MarkupCached]] = None

  def cached: MarkupCached = cachedVar match
    case None => readAndCache("Reading")
    case Some(reference) => reference.get match
      case None => readAndCache("Re-reading evicted")
      case Some(cached) => cached

  private def readAndCache(message: String): MarkupCached =
    site.log.debug(s"$message MarkupSource: $sourcePath")

    val (frontMatterOrError: Either[PageError, FrontMatter], markupContent: String) =
      FrontMatter.parse(sourcePath, Files.read(sourcePath.file(site.sourceDirectory)))

    val frontMatter: FrontMatter = frontMatterOrError match
      case Right(frontMatter) => frontMatter
      case Left(error) =>
        site.errors.error(error)
        FrontMatter.absent

    val xml: Xml.Element = markup.parse(sourcePath, markupContent) match
      case Right(xml) => xml
      case Left(error) =>
        site.errors.error(error)
        var result = Xml.element("div")
        result = Xml.ClassName.add(result, "malformed-xml")
        result = Xml.setText(result, s"Malformed XML: $error")
        result

    val result: MarkupCached = MarkupCached(
      markup = markup,
      errorReporter = PageError.Reporter(sourcePath, site),
      siteUrl = site.url,
      frontMatter = frontMatter,
      xml = xml
    )

    cachedVar = Some(SoftReference(result))
    result
