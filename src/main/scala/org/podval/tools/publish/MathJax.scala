package org.podval.tools.publish

import zio.blocks.html.*

object MathJax extends Html.JSLibrary:
  override def body: List[Html.Element] = List.empty

  override val head: List[Html.Element] = List(
    // TODO they recommend "defer" (or "async"?) attribute on the "script" tag
    // TODO they recommend sticking this in the "head", not body; maybe Highlights.js works that way too?
    // TODO use the same CDN for Highlights.js
    script().externalJs("https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js"),
    // TODO they insist that this must come before the script that loads MathJax;
    // maybe Highlights.js also works that way?
    script().inlineJs(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")
  )
