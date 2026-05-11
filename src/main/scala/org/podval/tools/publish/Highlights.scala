package org.podval.tools.publish

import zio.blocks.html.*
import zio.blocks.chunk.Chunk

object Highlights:
  val version = "11.11.1"

final class Highlights(languages: Set[String]) extends Html.JSLibrary:
  private val cdn: String = s"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/${Highlights.version}"

  override val head: List[Html.Element] =
    List(Html.stylesheet(s"$cdn/styles/default.min.css"))

  override val body: List[Html.Element] =
    List(script().externalJs(s"$cdn/highlight.min.js")) ++
    languages.map(language => script().externalJs(languageModule(language))) ++
    List(script().inlineJs(js"hljs.highlightAll();"))

  private def languageModule(language: String): String =
// NOT SUPPORTED   if language.toLowerCase == "liquid" then "https://unpkg.com/highlightjs-liquid@0.9.1/dist/liquid.min.js" else
      s"$cdn/languages/$language.min.js"


