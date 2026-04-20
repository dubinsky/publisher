package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

object Markup:
  val all: List[Markup] = List(
    MarkdownFlexMark,
    Html
  )
  
abstract class Markup(
  final val extension: String,
  additionalExtensions: Set[String]
) derives CanEqual:
  override def toString: String = extension

  private val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  def parse(sourcePath: Path, content: String): Either[PageError, Xml]

  def linkElementResolvers: Seq[LinkElementResolver]

