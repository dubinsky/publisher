package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import XmlUtil.{apply, el}

// TODO site first
final class Sitemap(
  site: Site,
  path: Path
) extends Page.SyntheticXml(
  site,
  path
):
  override def xml: Xml.Element = el("urlset")
    .attr(XmlName("xsi", Some("xmlns"), None), "http://www.w3.org/2001/XMLSchema-instance")
    .attr(XmlName("schemaLocation", Some("xsi")), "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd")
    (
      // TODO unordered, for each file; lastmod only if date (lastModified?) is present...
      //<url>
      //<loc>http://dub.podval.org/2009/08/07/sale-of-hometz.html</loc>
      //<lastmod>2009-08-07T14:30:00-04:00</lastmod>
      //</url>
    )
