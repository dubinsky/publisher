package org.podval.tools.publish

import org.typelevel.paiges.Doc
import zio.blocks.chunk.Chunk

final class XmlWriter[X <: XmlAst](val xml: X)(
  val config: XmlWriter.Config,
  val width: Int = 120,
  val indent: Int = 2
):
  def render(element: xml.Element): String =
    fromElement(
      element,
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(XmlWriter.hiddenNewline, "\n")
      .appended('\n')

  private def fromElement(
    element: xml.Element,
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    val attributeValues: Chunk[(String, String)] = xml.attributes(element, parent)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map((name, value) =>
        Doc.text(s"$name=") + Doc.lineOrEmpty + Doc.text(Strings.quote(encodeXmlSpecials(value)))
      ))

    val nodes: List[xml.Xml] = atomize(List.empty, xml.children(element).toList)
    val chunks: Seq[Seq[xml.Xml]] = chunkify(Seq.empty, List.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(!xml.isAtom(_)))
    val whitespaceLeft: Boolean = nodes.headOption.exists(isWhitespace)
    val whitespaceRight: Boolean = nodes.lastOption.exists(isWhitespace)
    val charactersLeft: Boolean = nodes.headOption.exists(isCharacters)
    val charactersRight: Boolean = nodes.lastOption.exists(isCharacters)
    
    val children: Seq[Doc] = if chunks.isEmpty then Seq.empty else
      val canBreakLeft1 = canBreakLeft || whitespaceLeft
      val canBreakRight1 = canBreakRight || whitespaceRight

       if chunks.length == 1 then Seq(
        fromChunk(chunks.head, Some(element), canBreakLeft1, canBreakRight1)
      ) else
        fromChunk(chunks.head, Some(element), canBreakLeft = canBreakLeft1, canBreakRight = true) +:
        chunks.tail.init.map(chunk => fromChunk(chunk, Some(element), canBreakLeft = true, canBreakRight = true)) :+
        fromChunk(chunks.last, Some(element), canBreakLeft = true, canBreakRight = canBreakRight1)

    val qName: String = xml.qName(element)

    if children.isEmpty then
      Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + (
        if config.selfClose(qName)
        then Doc.text("/>")
        else Doc.text(s"></$qName>")
      )
    else
      val start: Doc = Doc.text(s"<$qName") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$qName>")

      val stack: Boolean =
        noText &&
        !config.unStack(qName) &&
        ((children.length >= 2) || ((children.length == 1) && config.stack(qName)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))),
          Doc.hardLine,
          end
        ))
      else if config.nest(qName) then
        // If this is forced-nested element - nest it:
        Doc.intercalate(Doc.lineOrSpace, children).tightBracketBy(left = start, right = end, indent)
      else
        // Mixed content or non-break-off-able attachments on the side(s) cause flow-style;
        // character content should stick to the opening and closing tags:
        Doc.cat(Seq(
          start,
          if canBreakLeft && !charactersLeft then Doc.lineOrEmpty else Doc.empty,
          Doc.intercalate(Doc.lineOrSpace, children),
          if canBreakRight && !charactersRight then Doc.lineOrEmpty else Doc.empty,
          end
        ))
  
  @scala.annotation.tailrec
  private def atomize(result: List[xml.Xml], nodes: List[xml.Xml]): List[xml.Xml] = if nodes.isEmpty then result else
    val (atoms: List[xml.Xml], tail: List[xml.Xml]) = nodes.span(xml.isAtom)

    val resultNew: List[xml.Xml] =
      if atoms.isEmpty
      then result
      else result ++ processText(Seq.empty, Strings.squashBigWhitespace(atoms.map(xml.atomText).mkString("")))

    tail match 
      case Nil => resultNew
      case n :: ns => atomize(resultNew :+ n, ns)

  @scala.annotation.tailrec
  private def processText(result: Seq[xml.Xml], text: String): Seq[xml.Xml] = if text.isEmpty then result else
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val resultNew: Seq[xml.Xml] = if spaces.isEmpty then result else result :+ space
    val (word: String, tail2: String) = tail.span(_ != ' ')
    if word.isEmpty
    then resultNew
    else processText(resultNew :+ xml.mkText(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(
    result: Seq[Seq[xml.Xml]],
    current: List[xml.Xml],
    nodes: List[xml.Xml],
    flush: Boolean
  ): Seq[Seq[xml.Xml]] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match
      case (Nil    , Nil    )                                     => result
      case (_      , Nil    )                                     => chunkify(result, current     , Nil     , flush = true )
      case (Nil    , n :: ns) if  isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
      case (_      , n :: ns) if  isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
      case (Nil    , n :: ns) if !isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
      case (c :: cs, n :: ns) if !isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
      case (c :: _ , n :: ns) if !isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )
  
  private def cling(c: xml.Xml, n: xml.Xml): Boolean =
    (xml.isAtom(c) && xml.isAtom(n)) || // Note: after atomize(), this should not be possible - right?
    (xml.isAtom(c) && xml.isElement(n) && clingLeft(xml.atomText(c).last)) || // TODO exclude ;( and friends
    (xml.isElement(c) && xml.isAtom(n) && clingRight(xml.atomText(n).head)) ||  // TODO exclude ;( and friends
    (xml.isElement(n) && config.cling(xml.qName(xml.asElement(n))))
  
  private def clingLeft(char: Char): Boolean =
    val typeCode: Byte = Character.getType(char).toByte
    typeCode == Character.CONNECTOR_PUNCTUATION ||
    typeCode == Character.START_PUNCTUATION ||
    typeCode == Character.INITIAL_QUOTE_PUNCTUATION

  private def clingRight(char: Char): Boolean =
    val typeCode: Byte = Character.getType(char).toByte
    typeCode == Character.CONNECTOR_PUNCTUATION ||
    typeCode == Character.END_PUNCTUATION ||
    typeCode == Character.FINAL_QUOTE_PUNCTUATION ||
    typeCode == Character.OTHER_PUNCTUATION

  private def fromChunk(
    nodes: Seq[xml.Xml],
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    require(nodes.nonEmpty)
    if nodes.length == 1 then
      fromNode(nodes.head, parent, canBreakLeft, canBreakRight)
    else Doc.intercalate(Doc.empty,
      fromNode(nodes.head, parent, canBreakLeft, canBreakRight = false) +:
      nodes.tail.init.map(node => fromNode(node, parent, canBreakLeft = false, canBreakRight = false)) :+
      fromNode(nodes.last, parent, canBreakLeft = false, canBreakRight)
    )
  
  private def fromNode(
    node: xml.Xml,
    parent: Option[xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    if xml.isElement(node) then
      val element: xml.Element = xml.asElement(node)
      val qName: String = xml.qName(element)
      if config.preformat(qName) then
        Doc.text(preformatElement(element, parent).mkString(XmlWriter.hiddenNewline))
      else
        val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
        if canBreakRight && config.break(qName) then result + Doc.hardLine else result
    else if xml.isAtom(node) then Doc.text(encodeXmlSpecials(xml.atomText(node)))
    else Doc.paragraph(toString(node))

  private def preformatElement(element: xml.Element, parent: Option[xml.Element]): Seq[String] =
    val attributeValues: Chunk[(String, String)] = xml.attributes(element, parent)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map((name, value) => s"$name=${Strings.quote(value)}") // TODO escapeSpecials?
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      xml.children(element).flatMap(node => preformat(node, Some(element)))

    val qName: String = xml.qName(element)
    if children.isEmpty then Seq(s"<$qName$attributes/>")
    else if children.length == 1 then Seq(s"<$qName$attributes>${children.head}</$qName>")
    else Seq(s"<$qName$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$qName>")

  private def preformat(node: xml.Xml, parent: Option[xml.Element]): Seq[String] =
    if xml.isElement(node) then preformatElement(xml.asElement(node), parent)
    else if xml.isAtom(node) then preformat(xml.atomText(node))
    else preformat(toString(node))

  private def preformat(string: String): Seq[String] =
    Strings.encodeXmlSpecials(string)
    /* encodeXmlSpecials(string) */.split("\n").toSeq
  
  private def encodeXmlSpecials(string: String): String =
    if config.encodeXmlSpecials then Strings.encodeXmlSpecials(string) else string

  private def space: xml.Xml = xml.mkText(" ")

  // TODO see Node.toString(node) in OpenTorah
  private def toString(node: xml.Xml): String = xml.toString(node)

  private def isCharacters(node: xml.Xml): Boolean = xml.isAtom(node) && xml.atomText(node).trim.nonEmpty
  private def isWhitespace(node: xml.Xml): Boolean = xml.isAtom(node) && xml.atomText(node).trim.isEmpty

object XmlWriter:
  val xmlWriter: XmlWriter[BlocksXml.type] = XmlWriter(BlocksXml)(htmlWriterConfig)
  val htmlWriter: XmlWriter[BlocksHtml.type] = XmlWriter(BlocksHtml)(htmlWriterConfig)

  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

  abstract class Config:
    def encodeXmlSpecials: Boolean = false // TODO do not double-encode what you did not decode ;)

    //  if allowEmptyElements || keepEmptyElements.contains(name.localName)
    //  Some elements are mis-processed when they are empty, e.g. <script .../> ...
    //  ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    //  and in general, it's weird to expand the elements that are always empty...
    def selfClose(name: String): Boolean
    def stack(name: String): Boolean
    def unStack(name: String): Boolean
    def nest(name: String): Boolean
    def cling(name: String): Boolean
    def break(name: String): Boolean
    def preformat(name: String): Boolean

  private def htmlWriterConfig: Config = new Config:
    override def selfClose(name: String): Boolean = Set("br", "hr", "meta", "link", "img", "input").contains(name)
    override def stack(name: String): Boolean = Set("nav", "header", "main", "div").contains(name)
    override def unStack(name: String): Boolean = false
    override def nest(name: String): Boolean = false
    override def cling(name: String): Boolean = false // TODO name.localName == "a"?
    override def break(name: String): Boolean = false // TODO TEI: lb; HTML: br?!
    override def preformat(name: String): Boolean = name == "pre"
