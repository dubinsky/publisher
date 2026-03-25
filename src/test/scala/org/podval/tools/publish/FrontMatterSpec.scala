package org.podval.tools.publish

import zio.blocks.docs.ParseError
import zio.blocks.schema.yaml.{Yaml, YamlError}
import zio.{Scope, durationInt}
import zio.test.*

object FrontMatterSpec extends ZIOSpecDefault:
  override def aspects: zio.Chunk[TestAspectAtLeastR[TestEnvironment]] =
    if TestPlatform.isJVM then zio.Chunk(TestAspect.timeout(120.seconds), TestAspect.timed)
    else zio.Chunk(TestAspect.timeout(120.seconds), TestAspect.timed, TestAspect.sequential, TestAspect.size(10))

  private given CanEqual[ParseError, ParseError] = CanEqual.derived
  private given CanEqual[FrontMatter, FrontMatter] = CanEqual.derived

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
        (frontMatter, content) => assertTrue(
          frontMatter.sourceLines == 2,
          content == "# Hello\n"
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
        (frontMatter, content) => assertTrue(
          frontMatter.sourceLines == 5,
          content == "# Hello\n"
        )
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
          |---
          |# Hello
          |""".stripMargin,
        (frontMatter, _) => assertTrue(frontMatter.get("title") match
          case Some(Yaml.Scalar("Hello", _)) => true
          case _ => false
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
        error => assertTrue(error.getMessage.contains("must be a mapping"))
      )
    }
  )

