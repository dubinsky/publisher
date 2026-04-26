package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

abstract class PageBase(
  val path: Path,
  val frontMatter: FrontMatter
) derives CanEqual:
  override def equals(obj: Any): Boolean = obj match
    case that: PageBase => this.path == that.path
    case _ => false

  // TODO exact: Boolean
  final def is(url: String): Boolean = paths.exists(_.toString.endsWith(url))
  
  final def title: String = frontMatter.title.getOrElse(path.title)
  
  final def dateString: String = frontMatter.date.fold("")(_.toShortString)

  final def ref: Xml.Element = Link.ToPage(this).a("post-link")
  
  protected def paths: List[Path]

  def xml: Xml.Element
