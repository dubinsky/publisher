package org.podval.tools.publish

import zio.blocks.schema.yaml.YamlError
import zio.Scope
import zio.test.*
import java.time.LocalDate

object FrontMatterSpec extends ZIOSpecDefault:
  private given CanEqual[LocalDate, LocalDate] = CanEqual.derived

  def roundTrip(input: String): TestResult =
    val (parsed: Either[YamlError, FrontMatter], _) = FrontMatter.parse(input)
    assertTrue(parsed.isRight) && {
      def render(parsed: Either[YamlError, FrontMatter]): String = parsed.toOption.get.write
      val rendered: String = render(parsed)
      val (reparsed: Either[YamlError, FrontMatter], _) = FrontMatter.parse(rendered)
      assertTrue(
        reparsed.isRight,
        rendered == render(reparsed)
      )
    }

  def parse(input: String, verify: (FrontMatter, String) => TestResult): TestResult =
    val (parsed: Either[YamlError, FrontMatter], content: String) = FrontMatter.parse(input)
    assertTrue(parsed.isRight) && verify(parsed.toOption.get, content)

  def error(input: String, verify: YamlError => TestResult): TestResult =
    val (parsed: Either[YamlError, FrontMatter], _) = FrontMatter.parse(input)
    assertTrue(parsed.isLeft) && verify(parsed.swap.toOption.get)

  override def spec: Spec[TestEnvironment & Scope, Any] = suite("FrontMatter")(
    test("empty FrontMatter") {
      parse(
        """---
          |---
          |# Hello
          |""".stripMargin,
        (frontMatter, content) => assertTrue(content ==
        """
          |
          |# Hello
          |""".stripMargin,
        )
      )
    },
    test("non-empty FrontMatter") {
      parse(
        """---
          |title: Hello
          |date: 2026-03-22
          |tags: [yaml, markdown, test]
          |---
          |# Hello
          |""".stripMargin,
        (frontMatter, content) => assertTrue(content ==
        """
          |
          |
          |
          |
          |# Hello
          |""".stripMargin,
        )
      )
    },
    test("FrontMatter must be a mapping") {
      error(
        """---
          |[yaml, markdown, test]
          |---
          |# Hello
          |""".stripMargin,
        error => assertTrue(error.getMessage.contains("Must be a Yaml.Mapping"))
      )
    },
    test("round-trip without FrontMatter") {
      roundTrip("# Hello\n")
    },
    test("round-trip with FrontMatter") {
      roundTrip(
        """---
          |title: Hello
          |date: 2026-03-22
          |tags: [yaml, markdown, test]
          |---
          |# Hello
          |""".stripMargin
      )
    },
    test("FrontMatter keys") {
      parse(
        """---
          |title: Hello
          |date: 2026-03-22
          |tags: [yaml, markdown, test]
          |categories: important
          |---
          |# Hello
          |""".stripMargin,
        (frontMatter, _) => assertTrue(
          frontMatter.title == Some("Hello"),
          frontMatter.layout == None,
          frontMatter.tags == List("yaml", "markdown", "test"),
          frontMatter.categories == List("important"),
          frontMatter.date == Some(LocalDate.of(2026, 3, 22))
        )
      )
    }
  )

