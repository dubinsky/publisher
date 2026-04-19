package org.podval.tools.publish

import zio.Scope
import zio.blocks.schema.xml.Xml
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml => TestResult): TestResult =
    val parsed = Markdown.parse(Path.root, input)
    assertTrue(parsed.isRight) && verify(parsed.toOption.get)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("empty FrontMatter") {
      parse(
        """- l1
          |  * l1-1
          |- l12
          |""".stripMargin,
        xml =>

          assertTrue(
            true
          )
      )
    }
  )

