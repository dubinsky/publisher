package org.podval.tools.publish

object Markup:
  val all: List[Markup] = List(
    Markdown,
    HtmlLike.Html
  )

abstract class Markup derives CanEqual:
  def extension: String

  def additionalExtensions: Set[String]

  override def toString: String = extension

  private lazy val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  def recognizeWikiLinks: Boolean

  def recognizeBlocks: Boolean

  def stop(xml: XmlAst)(element: xml.Element): Boolean

  def isSectionElement(element: Xml.Element): Boolean

  def getSectionTitle(element: Xml.Element): Option[String]

  def getSections(element: Xml.Element, site: Site, sourcePath: Path): Seq[Page.Section]

  // This is where TEI link elements like `persName` get converted into HTML `a` elements
  def convertLinks(element: Xml.Element): Xml.Element
