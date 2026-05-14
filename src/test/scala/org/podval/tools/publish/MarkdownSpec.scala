package org.podval.tools.publish

import org.podval.xml.{Xml, XmlWriter}
import zio.Scope
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml.Element => TestResult): TestResult =
    val parsed = Markdown.parse(Path.root, input)
    assertTrue(parsed.isRight) && verify(parsed.toOption.get)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("nested lists") {
      parse(
        """* TOC
          |{:toc}
          |""".stripMargin,
        xml =>

          println(XmlWriter.xmlWriter.render(xml))
          assertTrue(
            true
          )
      )
    }
  )

