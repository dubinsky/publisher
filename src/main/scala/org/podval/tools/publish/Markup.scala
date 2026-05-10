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

  def stop(elementName: String): Boolean
  
  def isSectionElement(element: Xml.Element): Boolean

  def getSections(element: Xml.Element, site: Site, sourcePath: Path): Seq[Page.Section]

  // This is where TEI link elements like `persName` get converted into HTML `a` elements
  def convertLinks(element: Xml.Element): Xml.Element

  final def gather[A](
    element: Xml.Element,
    gatherElement: Xml.Element => Option[A]
  ): Seq[A] =
    def loop(element: Xml.Element): Seq[A] =
      if stop(Xml.qName(element)) then Seq.empty else
        gatherElement(element).toSeq ++
        Xml.children(element).flatMap(Xml.asElement).flatMap(element => loop(element))

    loop(element)

  final def gatherWithParents[A](
    element: Xml.Element,
    gatherElement: (Xml.Element, Seq[Xml.Element]) => Option[A]
  ): Seq[A] =
    def loop(element: Xml.Element, parents: Seq[Xml.Element]): Seq[A] =
      if stop(Xml.qName(element)) then Seq.empty else
        val parentsNew: Seq[Xml.Element] = element +: parents
        gatherElement(element, parents).toSeq ++
        Xml.children(element).flatMap(Xml.asElement).flatMap(element => loop(element, parentsNew))

    loop(element, Seq.empty)
