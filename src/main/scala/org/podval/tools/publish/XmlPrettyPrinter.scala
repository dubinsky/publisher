package org.podval.tools.publish

import org.typelevel.paiges.Doc
import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}

// TODO rename XmlWriter
object XmlPrettyPrinter:
  abstract class Config:
    //  if allowEmptyElements || keepEmptyElements.contains(name.localName)
    //  Some elements are mis-processed when they are empty, e.g. <script .../> ...
    //  ... except, some elements are mis-processed when they *are* non-empty (e.g., <br>),
    //  and in general, it's weird to expand the elements that are always empty...
    def selfClose(name: XmlName): Boolean
    def stack(name: XmlName): Boolean
    def unStack(name: XmlName): Boolean
    def nest(name: XmlName): Boolean
    def cling(name: XmlName): Boolean
    def break(name: XmlName): Boolean
    def preformat(name: XmlName): Boolean
    
  // The only way I found to not let Paiges screw up indentation in the <pre><code>..</code></pre> blocks
  // is to give it the whole block as one unbreakable text, and for that I need to hide newlines from it -
  // and then restore them in render()...
  // Also, element start and end tags must not be separated from the children by newlines...
  private val hiddenNewline: String = "\\n"

final class XmlPrettyPrinter(
  val config: XmlPrettyPrinter.Config,
  val width: Int = 120,
  val indent: Int = 2,
  val encodeXmlSpecials: Boolean = false // TODO do not double-encode what you did not decode ;)
):
  def render(element: Xml.Element): String =
    fromElement(
      element,
      parent = None,
      canBreakLeft = true,
      canBreakRight = true
    )
      .render(width)
      .replace(XmlPrettyPrinter.hiddenNewline, "\n")
      .appended('\n')

  private def fromElement(
    element: Xml.Element,
    parent: Option[Xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    val attributeValues: Chunk[(XmlName, String)] = getAttributeValues(element, parent)
    val attributes: Doc =
      if attributeValues.isEmpty then Doc.empty
      else Doc.lineOrSpace + Doc.intercalate(Doc.lineOrSpace, attributeValues.map((name, value) =>
        Doc.text(s"$name=") + Doc.lineOrEmpty + Doc.text(Strings.quote(encodeXmlSpecials(value)))
      ))

    val nodes: List[Xml] = atomize(List.empty, element.children.toList)
    val chunks: Seq[Seq[Xml]] = chunkify(Seq.empty, List.empty, nodes, flush = false)
    val noText: Boolean = chunks.forall(_.forall(!isAtom(_)))
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

    val name: String = element.name.toString

    if children.isEmpty then
      Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + (
        if config.selfClose(element.name)
        then Doc.text("/>")
        else Doc.text(s"></$name>")
      )
    else
      val start: Doc = Doc.text(s"<$name") + attributes + Doc.lineOrEmpty + Doc.text(">")
      val end: Doc = Doc.text(s"</$name>")

      val stack: Boolean =
        noText &&
        !config.unStack(element.name) &&
        ((children.length >= 2) || ((children.length == 1) && config.stack(element.name)))

      if stack then
        // If this is clearly a bunch of elements - stack 'em with an indent:
        Doc.cat(Seq(
          start,
          Doc.cat(children.map(child => (Doc.hardLine + child).nested(indent))),
          Doc.hardLine,
          end
        ))
      else if config.nest(element.name) then
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
  private def atomize(result: List[Xml], nodes: List[Xml]): List[Xml] = if nodes.isEmpty then result else
    val (atoms: List[Xml], tail: List[Xml]) = nodes.span(isAtom)

    val resultNew: List[Xml] =
      if atoms.isEmpty
      then result
      else result ++ processText(Seq.empty, Strings.squashBigWhitespace(atoms.map(atomText).mkString("")))

    tail match 
      case Nil => resultNew
      case n :: ns => atomize(resultNew :+ n, ns)

  @scala.annotation.tailrec
  private def processText(result: Seq[Xml], text: String): Seq[Xml] = if text.isEmpty then result else
    val (spaces: String, tail: String) = text.span(_ == ' ')
    val resultNew: Seq[Xml] = if spaces.isEmpty then result else result :+ space
    val (word: String, tail2: String) = tail.span(_ != ' ')
    if word.isEmpty
    then resultNew
    else processText(resultNew :+ Xml.Text(word), tail2)

  @scala.annotation.tailrec
  private def chunkify(
    result: Seq[Seq[Xml]],
    current: List[Xml],
    nodes: List[Xml],
    flush: Boolean
  ): Seq[Seq[Xml]] =
    if flush then chunkify(result :+ current.reverse, Nil, nodes, flush = false) else (current, nodes) match
      case (Nil    , Nil    )                                     => result
      case (_      , Nil    )                                     => chunkify(result, current     , Nil     , flush = true )
      case (Nil    , n :: ns) if  isWhitespace(n)                 => chunkify(result, Nil         , ns      , flush = false)
      case (_      , n :: ns) if  isWhitespace(n)                 => chunkify(result, current     , ns      , flush = true )
      case (Nil    , n :: ns) if !isWhitespace(n)                 => chunkify(result, n :: Nil    , ns      , flush = false)
      case (c :: cs, n :: ns) if !isWhitespace(n) &&  cling(c, n) => chunkify(result, n :: c :: cs, ns      , flush = false)
      case (c :: _ , n :: ns) if !isWhitespace(n) && !cling(c, n) => chunkify(result, current     , n :: ns , flush = true )
  
  private def cling(c: Xml, n: Xml): Boolean =
    (isAtom(c) && isAtom(n)) || // Note: after atomize(), this should not be possible - right?
    (isAtom(c) && isElement(n) && clingLeft(atomText(c).last)) || // TODO exclude ;( and friends
    (isElement(c) && isAtom(n) && clingRight(atomText(n).head)) ||  // TODO exclude ;( and friends
    (isElement(n) && config.cling(n.asInstanceOf[Xml.Element].name))
  
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
    nodes: Seq[Xml],
    parent: Option[Xml.Element],
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
    node: Xml,
    parent: Option[Xml.Element],
    canBreakLeft: Boolean,
    canBreakRight: Boolean
  ): Doc =
    if isElement(node) then
      val element: Xml.Element = node.asInstanceOf[Xml.Element]
      if config.preformat(element.name) then
        Doc.text(preformat(element, parent).mkString(XmlPrettyPrinter.hiddenNewline))
      else
        val result: Doc = fromElement(element, parent, canBreakLeft, canBreakRight)
        // Note: suppressing extra hardLine when lb is in a stack is non-trivial - and not worth it :)
        if canBreakRight && config.break(element.name) then result + Doc.hardLine else result
    else if isAtom(node) then Doc.text(encodeXmlSpecials(atomText(node)))
    else Doc.paragraph(toString(node))

  private def preformat(element: Xml.Element, parent: Option[Xml.Element]): Seq[String] =
    val attributeValues: Chunk[(XmlName, String)] = getAttributeValues(element, parent)
    val attributes: String = if attributeValues.isEmpty then "" else attributeValues
      .map((name, value) => s"$name=${Strings.quote(value)}") // TODO escapeSpecials?
      .mkString(" ", ", ", "")

    val children: Seq[String] =
      element.children.flatMap(node => preformat(node, Some(element)))

    val name: String = element.name.toString

    if children.isEmpty then Seq(s"<$name$attributes/>")
    else if children.length == 1 then Seq(s"<$name$attributes>${children.head}</$name>")
    else Seq(s"<$name$attributes>" + children.head) ++ children.tail.init ++ Seq(children.last + s"</$name>")

  private def preformat(node: Xml, parent: Option[Xml.Element]): Seq[String] =
    if isElement(node) then preformat(node.asInstanceOf[Xml.Element], parent)
    else if isAtom(node) then preformat(atomText(node))
    else preformat(toString(node))

  private def preformat(string: String): Seq[String] =
    Strings.encodeXmlSpecials(string)
    /* encodeXmlSpecials(string) */.split("\n").toSeq
  
  private def encodeXmlSpecials(string: String): String =
    if encodeXmlSpecials then Strings.encodeXmlSpecials(string) else string

  private def space: Xml.Text = Xml.Text(" ")

  // TODO see Node.toString(node) in OpenTorah
  private def toString(xml: Xml): String = XmlUtil.toString(xml)

  private def isElement(xml: Xml): Boolean = xml match
    case _: Xml.Element => true
    case _ => false

  private def getAttributeValues(element: Xml.Element, parent: Option[Xml.Element]): Chunk[(XmlName, String)] =
    // TODO deal with the namespaces - and defaults?!
    //    val parentNamespaces: Seq[Namespace] = parent.fold[Seq[Namespace]](Seq.empty)(Namespace.getAll)
    //    Namespace.getAll(element).filterNot(parentNamespaces.contains).map(_.attributeValue) ++
    //    Attribute.get(element).filterNot(_.value.isEmpty)
    element.attributes

  // TODO introduce Atom(value: String) destructor?
  private def isAtom(xml: Xml): Boolean = xml match
    case _: Xml.Text => true
    case _: Xml.CData => true
    case _ => false

  private def atomText(xml: Xml): String = xml match
    case Xml.Text(value) => value
    case Xml.CData(value) => value
    case xml => throw new IllegalArgumentException(s"Not an XML atom: $xml")

  private def isCharacters(xml: Xml): Boolean = isAtom(xml) && atomText(xml).trim.nonEmpty
  private def isWhitespace(xml: Xml): Boolean = isAtom(xml) && atomText(xml).trim.isEmpty
    
