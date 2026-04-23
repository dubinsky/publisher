package org.podval.tools.publish

abstract class SyntheticPage(
  targetPath: Path,
  pageKind: PageKind,
  frontMatter: FrontMatter
) extends PageBase(
  targetPath.withExtension(Html.extension),
  pageKind,
  frontMatter
):
  override def toString: String = s"$title.${Html.extension}($targetPath)"

  override def paths: List[Path] = List(
    targetPath,
    targetPath.withoutExtension
  )
