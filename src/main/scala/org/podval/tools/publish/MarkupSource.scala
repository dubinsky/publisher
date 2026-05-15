package org.podval.tools.publish

import org.podval.xml.Xml
import scala.ref.SoftReference

final class MarkupSource(
  val markup: Markup,
  val site: Site,
  val sourcePath: Path
):
  private var cachedVar: Option[SoftReference[MarkupCached]] = None

  private def cache(cached: MarkupCached): Unit = cachedVar = Some(SoftReference(cached))

  private def mk(xml: Xml.Element): MarkupCached = MarkupCached(
    markup = markup,
    errorReporter = PageError.Reporter(sourcePath, site),
    siteUrl = site.url,
    xml = xml
  )

  def cache(xml: Xml.Element): Unit = cache(mk(xml))

  def cached: MarkupCached = cachedVar.get.get.getOrElse:
    site.log.warn(s"Re-reading evicted PageMarkup: $sourcePath")
    val (frontMatter: FrontMatter, xml: Xml.Element) = site.parseMarkup(sourcePath, markup)
    val cached: MarkupCached = mk(xml)
    cache(cached)
    cached
 