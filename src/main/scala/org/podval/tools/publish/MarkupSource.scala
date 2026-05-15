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

  def cache(xml: Xml.Element): Unit = cache(MarkupCached(this, xml))

  def cached: MarkupCached = cachedVar.get.get.getOrElse:
    site.log.warn(s"Re-reading evicted PageMarkup: $sourcePath")
    val (frontMatter: FrontMatter, xml: Xml.Element) = site.parseMarkup(sourcePath, markup)
    val cached: MarkupCached = MarkupCached(this, xml)
    cache(cached)
    cached
