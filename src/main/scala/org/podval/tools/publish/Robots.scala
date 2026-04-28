package org.podval.tools.publish

final class Robots(
  site: Site,
  path: Path,
  sitemap: Sitemap
) extends Asset.Synthetic(
  site,
  path
):
  override def content: String = s"Sitemap: ${site.url}/${sitemap.path}"
