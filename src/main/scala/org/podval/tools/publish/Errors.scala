package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.*
import zio.blocks.schema.xml.Xml

final class Errors(
  site: Site,
  path: Path
) extends Page.SyntheticMarkupPage(
  site,
  path = path,
  frontMatter = FrontMatter(
    title = Some("Site Errors"),
    layout = Some("page"),
    description = Some("Site errors by kind"),
    //    permalink = Some(path.withoutExtension.toString)
  )
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override def xml: Xml.Element =
    // TODO generate report page(s):
    //- broken links
    //- inconsistent titles
    div("site-errors").setId("site-errors")()
