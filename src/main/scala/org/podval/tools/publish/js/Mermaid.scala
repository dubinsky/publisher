package org.podval.tools.publish.js

import org.podval.xml.Html
import zio.blocks.html.*

object Mermaid extends JSLibrary:
  override def head: List[Html.Element] = List.empty

  override def body: List[Html.Element] = List(
    script(`type` := "module").inlineJs(
      js"""import mermaid from 'https://cdn.jsdelivr.net/npm/mermaid@11/dist/mermaid.esm.min.mjs';
          |mermaid.initialize({ startOnLoad: false });
          |await mermaid.run({ querySelector: '.language-mermaid', });
          |""".stripMargin
    )
  )
