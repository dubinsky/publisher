package org.podval.tools.publish

import zio.blocks.docs.frontmatter.yaml.{DocWithYamlFrontmatter, Parser}
import java.io.File

final class MarkdownPage(
  override val path: Path,
  override val fromFile: File,
  val isIndex: Boolean = false // TODO deal with 'isIndex'
) extends Page.Rendered:
  override def redirectFrom: List[File] = List.empty // TODO consult the frontmatter 'permalink' tag
  def title: String = ??? // TODO file name, Markdown title, frontmatter title

  private lazy val docWithYamlFrontmatter: DocWithYamlFrontmatter =
    Parser.parse(Files.read(fromFile)).toOption.get
