package org.podval.tools.publish

import java.io.File

// Something that ends up on the site
trait Page:
  def path: Path
  override def toString: String = path.mkString("/", "/", "")
  override def hashCode(): Int = path.hashCode
  override def equals(obj: Any): Boolean = obj match
    case that: Page => this.path == that.path
    case _ => false

object Page:
  trait FromFile extends Page:
    def fromFile: File

  // Something that gets copied directly from the root into the site
  final class Copied(
    override val path: Path,
    override val fromFile: File
  ) extends FromFile:
    def copyTo(toFile: File): Unit = Files.copy(fromFile, toFile)

  // Something that ends up on the site as HTML
  trait Rendered extends FromFile:
    def redirectFrom: List[File]
//    def render: ???
