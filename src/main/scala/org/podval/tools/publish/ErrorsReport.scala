package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.*
import zio.blocks.schema.xml.Xml

final class ErrorsReport(
  targetPath: Path,
  site: Site
) extends SyntheticPage(
  targetPathSynthetic = targetPath,
  frontMatter = FrontMatter(
    title = Some("Site Errors"),
    layout = Some("page"),
    description = Some("Site errors by kind"),
    //    permalink = Some(targetPath.withoutExtension.toString)
  )
):
  private def slugify(text: String): String = text.replace(' ', '-')

  override def xml: Xml.Element =
    // TODO generate report page(s):
    //- broken links
    //- inconsistent titles
    div("site-errors").setId("site-errors")()
