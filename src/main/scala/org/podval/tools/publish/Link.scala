package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import XmlUtil.{childrenWhenEmpty, replaceAttribute, withText}

final case class Link(
  from: Link.From,
  to: Link.To
)

object Link:
  final case class SectionElement(
    level: Option[Int],
    id: Option[String],
    text: Option[String]
  )

  // TODO rename Ref?
  final case class FromElement(
    ref: String, // could be `name`, `path/name`(?), `name#section`, `name#section#subsection`, `name#^block`.
    text: Option[String],
    kind: Option[String] // TEI org/person/place, facsimile, etc.
  )

  final case class From(
    page: MarkupPage,
    fromElement: FromElement,
    context: Option[String],  // TODO retrieve link context
    // TODO section
    element: Option[Xml.Element],
    transclude: Boolean
  ):
    def isWikiLink: Boolean = element.isEmpty

  sealed abstract class To(val page: Page):
    final def url: String = page.path.toString + urlMore
    protected def urlMore: String
    final def text: String = page.title + textMore
    protected def textMore: String

    final def a(cls: String): Xml.Element = XmlUtil.a(cls, url).withText(text)

    final def a(element: Xml.Element): Xml.Element = element
      .copy(name = XmlUtil.a)
      .replaceAttribute(XmlUtil.hrefAttribute, XmlUtil.escapeUrl(url))
      .childrenWhenEmpty(Some(text))

  final case class ToPage(override val page: Page) extends To(page):
    override protected def urlMore: String = ""
    override protected def textMore: String = ""

  private final case class ToSection(override val page: Page, sections: Seq[Section]) extends To(page):
    override protected def urlMore: String = s"#${sections.last.id}"
    override protected def textMore: String = s"#${sections.map(_.title).mkString("#")}"

  private final case class ToBlock(override val page: Page, block: Block) extends To(page):
    override protected def urlMore: String = s"#${block.id}"
    override protected def textMore: String = s"#^${block.id}"

  // TODO reportError()
  def resolveRef(ref: String, site: Site): Option[Link.To] =
    val (toPath: String, fragment: Option[String]) = Files.split(ref, '#')

    site.pages.find(_.is(toPath)).flatMap: toPage =>
      fragment match
        case None => Some(ToPage(toPage))
        case Some(fragment) =>
          toPage match
            case toPage: MarkupPage =>
              if fragment.startsWith("^") then
                for
                  id: String = fragment.substring(1).trim
                  block: Block <- toPage.block(id)
                yield ToBlock(toPage, block)
              else
                for
                  names: Seq[String] = fragment.split('#').map(_.trim).toSeq
                  sections: Seq[Section] <- toPage.section(names)
                yield ToSection(toPage, sections)

            case toPage =>
              // TODO site.reportError()
              None
