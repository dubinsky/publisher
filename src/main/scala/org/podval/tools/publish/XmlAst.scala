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
  def rename(element: Element, name: String): Element

  // TODO deal with the namespaces - and defaults?!
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)
  def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)]
  def attributes(element: Element): Chunk[(String, String)]

  final def getAttribute(element: Element, name: String): Option[String] =
    attributes(element).find(_._1 == name).map(_._2)

  def setAttribute(element: Element, name: String, value: String): Element
  def children(element: Element): Chunk[Xml]
  def setChildren(element: Element, children: Chunk[Xml]): Element

  def mkText(text: String): Xml
  def isAtom(xml: Xml): Boolean
  def isText(xml: Xml): Boolean
  def atomText(xml: Xml): String

  // transformation
  final def transform(
    element: Element,
    delve: Element => Boolean,
    transformElement: Element => Element
  ): Element =
    def loop(element: Element): Element = if !delve(element) then element else
      transformElement(setChildren(element, children(element).map: xml =>
        if isElement(xml)
        then loop(asElement(xml))
        else xml
      ))

    loop(element)

  // data gathering
  final def gather[A](
    element: Element,
    delve: Element => Boolean,
    gatherElement: Element => Option[A]
  ): Seq[A] =
    def loop(element: Element): Seq[A] =
      if !delve(element)
      then Seq.empty
      else gatherElement(element).toSeq ++ children(element)
        .filter(isElement)
        .map(asElement)
        .flatMap(loop)

    loop(element)