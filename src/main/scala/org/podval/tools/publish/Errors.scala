package org.podval.tools.publish

import org.podval.tools.publish.XmlUtil.{apply, child, div, setId}
import zio.blocks.schema.xml.Xml

final class Errors(
  site: Site,
  path: Path,
  frontMatter: FrontMatter,
  pageMarkup: Option[PageMarkup]
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
  object Maker extends MarkupPage.AutoMaker[Errors](
    path = Path("errors").html,
    make = Errors.apply,
    frontMatterWithoutSource = FrontMatter(
      title = Some("Errors"),
      description = Some("Site errors by kind"),
      //    permalink = Some(path.withoutExtension.toString)
    )
  )
