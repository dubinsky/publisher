package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

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
