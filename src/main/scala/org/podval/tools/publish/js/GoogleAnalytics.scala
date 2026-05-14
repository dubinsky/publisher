package org.podval.tools.publish.js

import org.podval.xml.Html
import zio.blocks.html.*

final class GoogleAnalytics(id: String) extends JSLibrary:
  override def body: List[Html.Element] = List.empty

  override def head: List[Html.Element] = List(
    script().externalJs(s"https://www.googletagmanager.com/gtag/js?id=$id"),
    script().inlineJs(
      js"""window.dataLayer = window.dataLayer || [];
          |function gtag(){window.dataLayer.push(arguments);}
          |gtag('js', new Date());
          |gtag('config', '$id');
          |""".stripMargin
    )
  )
  