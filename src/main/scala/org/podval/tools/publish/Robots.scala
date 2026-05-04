package org.podval.tools.publish

object Robots:
  val path: Path = Path("robots").withExtension("txt")
  
final class Robots(
  site: Site
) extends Asset.Synthetic(
  site,
  Robots.path
):
  override def content: String = s"Sitemap: ${site.url}/${Sitemap.path}"
