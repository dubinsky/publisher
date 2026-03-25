package org.podval.tools.publish

import java.io.File

final class Path(
  val path: List[String],
  val extension: Option[String]
):
  override def equals(obj: Any): Boolean = obj match
    case that: Path => this.path == that.path && this.extension == that.extension
    
  override def toString: String = path.mkString("/", "/", extensionString)

  def startsWith(name: String): Boolean = path match
    case x :: xs => x == name
    case _ => false

  private def extensionString: String = extension match
    case None => ""
    case Some(extension) => s".$extension"

  def withoutExtension: Path = withExtension(None)
  def withExtension(extension: String): Path = withExtension(Some(extension))

  private def withExtension(extension: Option[String]): Path =
    if this.extension == extension
    then this
    else Path(path, extension)

  def file(directory: File): File = file(directory, path)
  
  @scala.annotation.tailrec
  private def file(directory: File, path: List[String]): File = path match
    case x :: Nil => File(directory, x + extensionString)
    case x :: xs => file(File(directory, x), xs)
    case Nil => directory

object Path:
  import scala.math.Ordered.orderingToOrdered
  given Ordering[Path] = (left: Path, right: Path) => Ordering[List[String]].compare(left.path, right.path)
  