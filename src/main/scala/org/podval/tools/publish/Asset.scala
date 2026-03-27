package org.podval.tools.publish

final class Asset(
  sourcePath: Path,
  targetPath: Path,
  pageKind: PageKind
) extends Page.WithSource(
  sourcePath,
  targetPath,
  pageKind
):
  override def toString: String = s"Asset($sourcePath, $targetPath)"

