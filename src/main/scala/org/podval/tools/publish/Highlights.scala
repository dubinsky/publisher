package org.podval.tools.publish

import zio.blocks.html.*
import zio.blocks.chunk.Chunk

final class Highlights(
  version: String,
  languages: Set[String]
) extends Html.JSLibrary:
  private val cdn: String = s"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/$version"

  override val head: List[Html.Element] =
    List(Html.stylesheet(s"$cdn/styles/default.min.css"))

  override val body: List[Html.Element] =
    List(script().externalJs(s"$cdn/highlight.min.js")) ++
    languages.map(language => script().externalJs(languageModule(language))) ++
    List(script().inlineJs(js"hljs.highlightAll();"))

  private def languageModule(language: String): String =
// NOT SUPPORTED   if language.toLowerCase == "liquid" then "https://unpkg.com/highlightjs-liquid@0.9.1/dist/liquid.min.js" else
      s"$cdn/languages/$language.min.js"

object Highlights:
  val version = "11.11.1"

  def get(xml: Seq[Html.Xml]): Option[Highlights] =
    val languages = xml.flatMap(getLanguages).toSet
    if languages.isEmpty
    then None
    else Some(Highlights(version, languages))

  private val languagePrefix: String = "language-"
  
  // Note: using ZIO Blocks HTML's CSS-based query to get
  // a list of languages used instead of recursing through the document :)
  private def getLanguages(xml: Html.Xml): Set[String] = xml
    .select(CssSelector.Element("code"))
    .attrs("class")
    .flatMap(_
      .split(" ")
      .map(_.trim)
      .find(_.startsWith(languagePrefix))
      .map(_.substring(languagePrefix.length))
    )
    .toSet

