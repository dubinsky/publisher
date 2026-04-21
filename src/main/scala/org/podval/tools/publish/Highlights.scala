package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlName}
import XmlUtil.{getAttribute, module, script, stylesheet}

final class Highlights(
  version: String,
  languages: Set[String]
) extends XmlUtil.JavascriptLibrary:
  private val cdn: String = s"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/$version"

  override val head: List[Xml.Element] = List(stylesheet(s"$cdn/styles/default.min.css"))

  override val body: List[Xml.Element] =
    List(module(s"$cdn/highlight.min.js")) ++
    languages.map(language => module(s"$cdn/languages/$language.min.js")) ++
    List(script("hljs.highlightAll();"))

object Highlights:
  val version = "11.11.1"

  def get(xml: Xml): Option[Highlights] =
    val languages = getLanguages(xml)
    if languages.isEmpty
    then None
    else Some(Highlights(version, languages))

  private given CanEqual[XmlName, XmlName] = CanEqual.derived

  private val languagePrefix: String = "language-"
  
  private def getLanguages(xml: Xml): Set[String] = xml match
    case element: Xml.Element =>
      if element.name != XmlUtil.code then element.children.flatMap(getLanguages).toSet else
        val result = for
          classes <- element.getAttribute(XmlUtil.`class`)
          language <- classes
            .split(" ")
            .map(_.trim)
            .find(_.startsWith(languagePrefix))
            .map(_.substring(languagePrefix.length))
        yield
          Set(language)
        result.getOrElse(Set.empty)
    case xml => Set.empty    
