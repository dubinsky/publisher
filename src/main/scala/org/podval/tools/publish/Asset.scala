package org.podval.tools.publish

final class Asset(
  sourcePath: Path,
  targetPath: Path,
  kind: PageKind
) extends Page(
  sourcePath,
  targetPath,
  kind: PageKind
):
  override def toString: String = s"Asset($sourcePath, $targetPath)"

