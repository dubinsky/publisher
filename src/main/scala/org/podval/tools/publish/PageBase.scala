package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

abstract class PageBase(
  val targetPath: Path,
  val frontMatter: FrontMatter
) derives CanEqual:
  override def equals(obj: Any): Boolean = obj match
    case that: PageBase => this.targetPath == that.targetPath
    case _ => false

  // TODO exact: Boolean
  final def is(url: String): Boolean = paths.exists(_.toString.endsWith(url))
  
  final def title: String = frontMatter.title.getOrElse(targetPath.title)
  
  final def dateString: String = frontMatter.date.fold("")(_.toShortString)

  protected def paths: List[Path]

  def xml: Xml.Element
