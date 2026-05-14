package org.podval.tools.publish.pages

import org.podval.tools.publish.{Page, Path, Site}

object Robots:
  val path: Path = Path("robots").withExtension("txt")
  
final class Robots(site: Site) extends Page.SyntheticAsset(
  site,
  Robots.path
):
  override def content: String = s"Sitemap: ${site.url}/${Sitemap.path}"
