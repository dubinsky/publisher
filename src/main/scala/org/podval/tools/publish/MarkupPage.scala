package org.podval.tools.publish

import org.podval.tools.publish.html.Html

final class MarkupPage(
  sourcePath: Path,
  targetPath: Path,
  kind: PageKind,
  frontMatter: FrontMatter,
  val markup: Markup,
  val ast: markup.AST,
) extends Page(
  sourcePath,
  targetPath.withExtension(Html.extension),
  kind
):
  def hasFrontMatter: Boolean = !frontMatter.isAbsent

  override def toString: String = s"MarkupPage[$markup]($sourcePath, $targetPath)"
