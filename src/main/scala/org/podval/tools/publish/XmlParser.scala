package org.podval.tools.publish

import zio.blocks.chunk.Chunk
import zio.blocks.schema.xml.{Xml, XmlName}
import scala.collection.mutable
import scala.jdk.CollectionConverters.IteratorHasAsScala
import java.io.StringReader
import javax.xml.namespace.QName
import javax.xml.stream.{XMLEventReader, XMLInputFactory, XMLStreamException}
import javax.xml.stream.events.{Attribute, Characters, Comment, EndElement, EntityReference, ProcessingInstruction,
  StartElement}

object XmlParser:
  def parse(content: String): Either[Throwable, Xml.Element] =
    try Right(parseInternal(content))
    catch case e: XMLStreamException => Left(e)

  private def parseInternal(content: String): Xml.Element =
    val factory: XMLInputFactory = XMLInputFactory.newInstance
    factory.setProperty(XMLInputFactory.IS_REPLACING_ENTITY_REFERENCES, false)
//    factory.setProperty(XMLInputFactory.IS_COALESCING, true)
    val reader: XMLEventReader = factory.createXMLEventReader(new StringReader(content))

    val elements: mutable.Stack[Xml.Element] = mutable.Stack.empty

    def addChild(child: Xml): Unit =
      val parent: Xml.Element = elements.pop()
      elements.push(parent.copy(children = parent.children :+ child))

    val characters: mutable.StringBuilder = mutable.StringBuilder()
    def addCharacters(more: String): Unit = characters.addAll(more)
    def flushCharacters(): Unit = if characters.nonEmpty then
      addChild(Xml.Text(characters.toString))
      characters.clear()

    var done: Boolean = false
    while !done && reader.hasNext do reader.nextEvent match
      case startElement: StartElement =>
        flushCharacters()
        elements.push(Xml.Element(
          name = fromQName(startElement.getName),
          children = Chunk.empty,
          attributes = Chunk.from(
            startElement.getAttributes.asScala.map(fromAttribute) ++
            startElement.getNamespaces.asScala.map(fromAttribute)
          ),
        ))

      case endElement: EndElement =>
        flushCharacters()
        if elements.length > 1
        then addChild(elements.pop())
        else done = true

      case characters: Characters =>
        val text: String = characters.getData
        if !characters.isCData then addCharacters(text) else
          flushCharacters()
          addChild(Xml.CData(text))

      case entityReference: EntityReference =>
        addCharacters(s"&${entityReference.getName};")

      case comment: Comment =>
        flushCharacters()
        addChild(Xml.Comment(comment.getText))

      case processingInstruction: ProcessingInstruction =>
        flushCharacters()
        addChild(Xml.ProcessingInstruction(
          target = processingInstruction.getTarget,
          data = processingInstruction.getData
        ))

      case xmlEvent => ()

    elements.pop()

  private def fromQName(qName: QName): XmlName = XmlName(
    localName = qName.getLocalPart,
    prefix = noneIfEmpty(qName.getPrefix),
    namespace = noneIfEmpty(qName.getNamespaceURI)
  )

  private def noneIfEmpty(string: String): Option[String] =
    Option.when(string.nonEmpty)(string)

  // Note: this takes care of the namespaces too - `Namespace` is derived from `Attribute`,
  // and `NamespaceImpl` handles the `xmlns:` prefix.
  private def fromAttribute(attribute: Attribute): (XmlName, String) = (
    fromQName(attribute.getName),
    attribute.getValue
  )
