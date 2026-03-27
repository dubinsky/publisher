package org.podval.tools.publish

trait Page:
  def targetPath: Path
  final def path: Path = targetPath.withoutExtension
  def sourcePath: Path
  def isSynthetic: Boolean

object Page:
  abstract class WithSource(
    final val targetPath: Path,
    final val sourcePath: Path,
    final val kind: PageKind
  ) extends Page:
    final override def isSynthetic: Boolean = false
  
  trait Synthetic extends Page:
    final override def isSynthetic: Boolean = true
    final override def sourcePath: Path = targetPath
    
