package org.podval.tools.publish

abstract class SyntheticPage(
  path: Path,
  frontMatter: FrontMatter
) extends PageBase(
  path,
  frontMatter
):
  override def toString: String = s"$title.${Html.extension}($path)"

  override protected def paths: List[Path] = List(
    path,
    path.withoutExtension
  )
