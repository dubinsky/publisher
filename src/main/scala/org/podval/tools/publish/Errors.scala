package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.{apply, child, div, setId}
import zio.blocks.schema.xml.Xml

final class Errors(
  site: Site,
  path: Path,
  pageMarkup: Option[PageMarkup],
  frontMatter: FrontMatter
) extends MarkupPage(
  site,
  path,
  frontMatter,
  pageMarkup,
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override protected def syntheticContent(content: Option[Xml.Element]): Xml.Element =
    // TODO incorporate content
    // TODO generate report page(s):
    //- broken links
    //- inconsistent titles
    div("site-errors").setId("site-errors").child(content)()

object Errors:
  final class Maker(site: Site, path: Path) extends MarkupPage.AutoMaker[Errors](site, path):
    override def withSource(pageMarkup: PageMarkup, frontMatter: FrontMatter): Errors =
      Errors(site, path, Some(pageMarkup), frontMatter)

    override def withoutSource: Errors =
      Errors(site, path, None, FrontMatter(
        title = Some("Site Errors"),
        description = Some("Site errors by kind"),
        //    permalink = Some(path.withoutExtension.toString)
      ))
