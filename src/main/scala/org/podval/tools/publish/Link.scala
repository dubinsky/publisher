package org.podval.tools.publish

import zio.blocks.schema.xml.XmlName

final case class Link(
  from: Link.From,
  toPage: Page
)

object Link:
  final case class From(
    page: Page,
    ref: String, // could be `name`, `path/name`, `name#section`, `name#section#subsection`, `name#^block`...
    category: Option[String],  // TEI org/person/place, facsimile, etc.
    context: Option[String],  // TODO retrieve link context
    // TODO section
  )

  abstract class ElementResolver(
    val elementName: XmlName,
    val refAttributeName: XmlName,
    val category: Option[String]
  )
  
  sealed trait Resolved:
    def url: String
    def text: String
    
  object Resolved:
    final case class ToPage(page: Page) extends Resolved:
      override def url: String = page.targetPath.toString
      override def text: String = page.title
      
    final case class ToSection(page: Page, sections: Seq[Toc.Section]) extends Resolved:
      override def url: String = s"${page.targetPath.toString}#${sections.last.id}"
      override def text: String = s"${page.title}#${sections.map(_.title).mkString("#")}"
      
//    final case class ToBlock(page: Page, ) extends Resolved // TODO block!
