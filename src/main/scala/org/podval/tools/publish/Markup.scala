package org.podval.tools.publish

import org.podval.tools.publish.html.Html
import org.podval.tools.publish.markdown.Markdown

abstract class Markup(
  final val extension: String,
  additionalExtensions: Set[String]
):
  override def toString: String = extension

  private val extensions: Set[String] = Set(extension) ++ additionalExtensions

  final def isExtension(extension: String): Boolean = extensions.contains(extension)

  type AST

  def read(content: String): AST

object Markup:
  val all: List[Markup] = List(
    Markdown,
    Html
  )
