package org.podval.tools.publish

import zio.blocks.html.*

object MathJax extends Html.JSLibrary:
  override def body: List[Html.Element] = List.empty

  override val head: List[Html.Element] = List(
    script().externalJs("https://cdn.jsdelivr.net/npm/mathjax@4/tex-mml-chtml.js"),
    script().inlineJs(js"MathJax = { tex: { inlineMath: {'[+]': [['$$', '$$']]} } };")
  )
