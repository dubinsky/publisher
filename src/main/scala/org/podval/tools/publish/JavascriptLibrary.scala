package org.podval.tools.publish

import zio.blocks.schema.xml.{Xml, XmlBuilder}

abstract class JavascriptLibrary:
  def head: List[Xml.Element]
  def body: List[Xml.Element]

object JavascriptLibrary:
  private def stylesheet(href: String) = XmlBuilder
    .element("link")
    .attr("rel", "stylesheet")
    .attr("href", href)
    .build
  
  private def script(text: String) = XmlBuilder
    .element("script")
    .child(XmlBuilder.text(text))
    .build
  
  private def module(src: String) = XmlBuilder
    .element("script")
    .attr("src", src)
    .child(XmlBuilder.comment("self-closing script elements do not work"))
    .build
    
  abstract class Highlights(version: String) extends JavascriptLibrary:
    private val cdn: String = s"https://cdnjs.cloudflare.com/ajax/libs/highlight.js/$version"

    private val languages: List[String] = List(
      "scala", "java", "gradle", "groovy", "ruby",
      "xml", "json", "yaml", "properties", "dockerfile",
      "typescript", "javascript", "css",
      "markdown", "asciidoc", "latex",
      "bash", "shell", "go",
    )

    override val head: List[Xml.Element] = List(stylesheet(s"$cdn/styles/default.min.css"))

    override val body: List[Xml.Element] =
      List(module(s"$cdn/highlight.min.js")) ++ 
      languages.map(language => module(s"$cdn/languages/$language.min.js")) ++
      List(script("hljs.highlightAll();"))
  
  object Highlights extends Highlights("11.11.1")

  object MathJax extends JavascriptLibrary:
    override def body: List[Xml.Element] = List.empty
    
    override val head: List[Xml.Element] = List(
      // TODO they recommend "defer" (or "async"?) attribute on the "script" tag
      // TODO they recommend sticking thin in the "head", not body; maybe Highlights.js works that way too?
      // TODO use the same CDN for Highlights.js
      module("https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js"),
      // TODO they insist that this must come before the script that loads MathJax;
      // maybe Highlights.js also works that way?
      script("MathJax = { tex: { inlineMath: {'[+]': [['$', '$']]} } };")
    )
