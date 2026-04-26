package org.podval.tools.publish

abstract class SyntheticPage(
  targetPathSynthetic: Path,
  frontMatter: FrontMatter
) extends PageBase(
  targetPathSynthetic.withExtension(Html.extension),
  frontMatter
):
  override def toString: String = s"$title.${Html.extension}($targetPath)"

  override protected def paths: List[Path] = List(
    targetPath,
    targetPath.withoutExtension
  )
