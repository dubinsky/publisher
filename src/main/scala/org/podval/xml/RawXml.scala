package org.podval.xml

import zio.blocks.schema.xml.{Xml, XmlCodec}

// I need to be able to work with *partially* decoded XML trees,
// with some subtrees held as raw XML.
// The only thing I need to be able to do with such undecoded subtrees is
// insert them into the result on encoding.
// This class serves this need:
// a field of type `RawXml` gets decoded from an XML element
// with the name corresponding to the field name,
// so name check is done by the core XML Codec;
// instance of this class holds the undecoded element until
// encoding, when it incorporates it into the output as-is.

// I have to use a member and not a constructor parameter to hold
// the undecoded XML tree, otherwise ZIO Block Schema deriver chokes with:
//    Found:    zio.blocks.schema.Schema[IndexedSeq[(zio.blocks.schema.xml.XmlName, String)]]
//    Required: zio.blocks.schema.Schema[zio.blocks.chunk.Chunk[(zio.blocks.schema.xml.XmlName, String)]]
//
//    The following import might make progress towards fixing the problem:
//      import zio.blocks.schema.OpticsFor.derive
//      given schema: Schema[Tei] = Schema.derived
class RawXml:
  private var _element: Option[Xml.Element] = None
  def set(element: Xml.Element): Unit = _element = Some(element)
  def get: Xml.Element = _element.get

object RawXml:
  def apply(element: Xml.Element): RawXml =
    val result = new RawXml
    result.set(element)
    result
    
  def codec: XmlCodec[RawXml] = new XmlCodec[RawXml]:
    def encodeValue(rawXml: RawXml): Xml = rawXml.get

    def decodeValue(xml: Xml): RawXml = xml match
      case element: Xml.Element => RawXml(element)
      case _ => error("Expected an element")
      
    

