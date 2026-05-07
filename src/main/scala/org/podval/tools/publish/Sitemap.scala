package org.podval.tools.publish

object Sitemap:
  val path: Path = Path("sitemap").withExtension("xml")
  
// TODO site first
final class Sitemap(
  site: Site
) extends Asset.SyntheticXml(
  site,
  Sitemap.path
):
  override def xmlContent: Xml.Element = Xml
    .element("urlset")
    .attr(zio.blocks.schema.xml.XmlName("xsi", Some("xmlns"), None), "http://www.w3.org/2001/XMLSchema-instance")
    .attr(zio.blocks.schema.xml.XmlName("schemaLocation", Some("xsi")), "http://www.sitemaps.org/schemas/sitemap/0.9 http://www.sitemaps.org/schemas/sitemap/0.9/sitemap.xsd")
    .children(
      // TODO unordered, for each file; lastmod only if date (lastModified?) is present;
      // directories are included, even the top level;
      // only HTML files are included, not the CSS
      //<url>
      //<loc>http://dub.podval.org/2009/08/07/sale-of-hometz.html</loc>
      //<lastmod>2009-08-07T14:30:00-04:00</lastmod>
      //</url>
    )
    .build
