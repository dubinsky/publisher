package org.podval.tools.publish

import zio.Scope
import zio.blocks.schema.xml.Xml
import zio.test.*

object MarkdownSpec extends ZIOSpecDefault:
  def parse(input: String, verify: Xml => TestResult): TestResult =
    val parsed = MarkdownFlexMark.parse(Path.root, input)
    assertTrue(parsed.isRight) && verify(parsed.toOption.get)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("Markdown")(
    test("nested lists") {
      parse(
        """- l1
          |  * l1-1
          |- l12
          |""".stripMargin,
        xml =>

          // TODO verify
          assertTrue(
            true
          )
      )
    }
  )

