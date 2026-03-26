package org.podval.tools.publish

abstract class Page(
  final val sourcePath: Path,
  final val targetPath: Path,
  final val kind: PageKind
):
  final def path: Path = targetPath.withoutExtension
