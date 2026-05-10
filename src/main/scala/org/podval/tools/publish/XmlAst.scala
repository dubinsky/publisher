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

  def transform(
    element: Element,
    stop: Element => Boolean,
    transformElement: Element => Element
  ): Element =
    def loop(element: Element): Element = if stop(element) then element else
      val result: Element = transformElement(element)
      setChildren(result, children(result).map(xml =>
        if isElement(xml)
        then loop(asElement(xml))
        else xml
      ))

    loop(element)
    
  object IdAttribute:
    val attributeName: String = "id"
    def toId(text: String): String = text.trim.replace(' ', '-')
    def get(element: Element): Option[String] = getAttribute(element, attributeName)
    def set(element: Element, value: String): Element = setAttribute(element, attributeName, value)

  object ClassAttribute:
    val attributeName: String = "class"

    def get(element: Element): Option[String] = getAttribute(element, attributeName)
    def set(element: Element, value: String): Element = setAttribute(element, attributeName, value)
    def set(element: Element, values: List[String]): Element = set(element, values.mkString(" "))
    def has(element: Element, name: String): Boolean = getList(element).contains(name)

    // TODO use in Highlights - better, retrieve languages in Minima itself, to deal with Mermaid etc.
    def getList(element: Element): List[String] = get(element).fold(List.empty)(_
      .split(' ')
      .toList
      .map(_.trim)
      .filterNot(_.isEmpty)
    )
    
    def add(element: Element, name: String): Element =
      val list = getList(element)
      if list.contains(name)
      then element
      else set(element, list.appended(name))
     