package org.podval.tools.publish

import zio.blocks.html.*

final class GoogleAnalytics(id: String) extends Html.JSLibrary:
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

  override def body: List[Html.Element] = List.empty
