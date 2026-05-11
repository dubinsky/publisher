package org.podval.tools.publish

import zio.blocks.chunk.Chunk

abstract class XmlAst:
  type Xml
  
  type Element <: Xml

  // Remove markup
  final def toStringOpt(element: Element): Option[String] =
    Option.when(children(element).nonEmpty)(toString(element))

  final def toString(xml: Xml): String = asAtom(xml)
    .orElse(asElement(xml).map(element => children(element).map(toString).mkString(" ")))
    .getOrElse("")

  def asElement(xml: Xml): Option[Element]
  def qName(element: Element): String
  def element(name: String): Element

  // TODO deal with the namespaces - and defaults?!
  //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
  //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
  //    Attribute.get(element).filterNot(_.value.isEmpty)
  def attributes(element: Element, parent: Option[Element]): Chunk[(String, String)]
  def attributes(element: Element): Chunk[(String, String)]

  def setAttribute(element: Element, name: String, value: String): Element
  def children(element: Element): Chunk[Xml]

  final def setText(element: Element, text: String): Element = setChildren(element, Chunk(mkText(text)))

  def setChildren(element: Element, children: Chunk[Xml]): Element

  def mkText(text: String): Xml
  def asAtom(xml: Xml): Option[String]
  def asText(xml: Xml): Option[String]

  def transform(
    element: Element,
    stop: String => Boolean,
    transformElement: Element => Element
  ): Element =
    def loop(element: Element): Element = if stop(qName(element)) then element else
      val result: Element = transformElement(element)
      setChildren(result, children(result).map(xml => asElement(xml).fold(xml)(loop)))

    loop(element)

  object A:
    val elementName: String = "a"
    def is(element: Element): Boolean = qName(element) == elementName

  sealed abstract class Attribute(val attributeName: String):
    final def get(element: Element): Option[String] = attributes(element).find(_._1 == attributeName).map(_._2)
    final def set(element: Element, value: String): Element = setAttribute(element, attributeName, value)

  object Id extends Attribute("id"):
    def toId(text: String): String = text.trim.replace(' ', '-')

  object Href extends Attribute("href")

  object ClassName extends Attribute("class"):
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

  abstract class ClassName(name: String):
    final def add(element: Element): Element = ClassName.add(element, name)
    final def has(element: Element): Boolean = ClassName.has(element, name)

  abstract class ClassNamePrefix(prefix: String):
    final def add(element: Element, name: String): Element = ClassName.add(element, s"$prefix-$name")
    final def get(element: Element): List[String] = ClassName.getList(element)
      .filter(_.startsWith(s"$prefix-"))
      .map(_.substring(prefix.length+1))
