package org.podval.tools.publish

abstract class Page(
  final val sourcePath: Path,
  final val targetPath: Path,
  final val kind: PageKind
):
  override def toString: String = s"${getClass.getName} $sourcePath => $targetPath"

  final def path: Path = targetPath.withoutExtension

