package org.podval.tools.publish.js

import org.podval.xml.Html

abstract class JSLibrary:
  def head: List[Html.Element]
  def body: List[Html.Element]

