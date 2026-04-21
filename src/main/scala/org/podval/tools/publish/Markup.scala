package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}

object Markup:
  val all: List[Markup] = List(
    Markdown,
    Html
  )

abstract class Markup(
  final val extension: String,
  additionalExtensions: Set[String],
  val doNotResolveLinksElements: Set[XmlName]
) derives CanEqual:
  override def toString: String = extension

  private val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml.Element]

  final def doResolveLinks(element: Xml.Element): Boolean = !doNotResolveLinksElements.contains(element.name)
  
  def linkElementResolvers: Seq[Link.ElementResolver]

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  final def linkElementResolver(element: Xml.Element): Option[Link.ElementResolver] =
    linkElementResolvers.find(_.elementName == element.name)
