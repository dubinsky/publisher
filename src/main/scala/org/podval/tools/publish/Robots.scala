package org.podval.tools.publish

final class Robots(
  site: Site,
  path: Path,
  sitemap: Sitemap
) extends Page.SyntheticText(
  site,
  path
):
  override def content: String = s"Sitemap: ${site.url}/${sitemap.path}"
