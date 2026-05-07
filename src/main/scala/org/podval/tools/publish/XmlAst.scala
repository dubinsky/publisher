package org.podval.tools.publish

import zio.blocks.chunk.Chunk

abstract class XmlAst:
  type Xml
  type Element <: Xml

  // Remove markup
  final def toStringOpt(element: Element): Option[String] =
    Option.when(children(element).nonEmpty)(toString(element))

  final def toString(xml: Xml): String =
    if isAtom(xml) then atomText(xml)
    else if isElement(xml) then children(asElement(xml)).map(toString).mkString(" ")
    else ""

  def isElement(xml: Xml): Boolean
  def asElement(xml: Xml): Element
  def qName(element: Element): String
  def children(element: Element): Chunk[Xml]

  // TODO deal with the namespaces - and defaults?!
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)
  def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)]

  def mkText(text: String): Xml
  def isAtom(xml: Xml): Boolean
  def isText(xml: Xml): Boolean
  def atomText(xml: Xml): String
