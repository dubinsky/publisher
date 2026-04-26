package org.podval.tools.publish

import java.io.File

final case class Path(
  path: Seq[String],
  extension: Option[String] = None
) derives CanEqual:
  def fileName: String = path.last
  def isIndex: Boolean = fileName == "index" && path.length > 1
  // TODO retrieve the parent title
  def title: String = if isIndex then path.init.last else fileName
  
  override def equals(obj: Any): Boolean = obj match
    case that: Path => this.path == that.path && this.extension == that.extension
    case _ => false
    
  override def toString: String = path.mkString("/", "/", extensionString)

  def startsWith(names: List[String]): Boolean = path.take(names.length) == names

  private def extensionString: String = extension match
    case None => ""
    case Some(extension) => s".$extension"

  def withoutExtension: Path = withExtension(None)
  def withExtension(extension: String): Path = withExtension(Some(extension))

  private def withExtension(extension: Option[String]): Path =
    if this.extension == extension
    then this
    else this.copy(extension = extension)

  def file(directory: File): File = file(directory, path)
  
  @scala.annotation.tailrec
  private def file(directory: File, path: Seq[String]): File =
    if path.isEmpty then directory
    else if path.length == 1 then File(directory, path.head + extensionString)
    else file(File(directory, path.head), path.tail)

object Path:
  val root: Path = new Path(Seq.empty, None)
  
  def apply(path: String*) = new Path(path, None)
  
  import scala.math.Ordered.orderingToOrdered
  given Ordering[Path] = (left: Path, right: Path) => Ordering[Seq[String]].compare(left.path, right.path)
  