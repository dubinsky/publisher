package org.podval.tools.publish

object Strings:
  def split(what: String, on: Char): (String, Option[String]) = what.lastIndexOf(on) match
    case -1 => (what, None)
    case index => (what.substring(0, index), Some(what.substring(index + 1)))

  def squashBigWhitespace(what: String): String = what
    .replace('\n', ' ')
    .replace('\t', ' ')

  def encodeXmlSpecials(string: String): String = string
    .replaceAll("&", "&amp;")
    .replaceAll("<", "&lt;")

  def escape(s: String): String = s
    .replace("&", "&amp;")
    .replace("<", "&lt;")
    .replace(">", "&gt;")
    .replace("\"", "&quot;")
    .replace("'", "&apos;")

  // Note: maybe use single quotes if the value contains double quote?
  def quote(value: String):String = "\"" + encodeXmlSpecials(value) + "\""