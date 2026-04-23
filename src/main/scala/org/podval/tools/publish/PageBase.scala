package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

abstract class PageBase(
  val targetPath: Path,
  val pageKind: PageKind,
  val frontMatter: FrontMatter
) derives CanEqual:
  override def equals(obj: Any): Boolean = obj match
    case that: PageBase => this.targetPath == that.targetPath
    case _ => false

  final def is(url: String): Boolean = paths.exists(_.toString.endsWith(url))

  final def title: String = frontMatter.title.getOrElse(targetPath.path.last)

  def paths: List[Path]

  def xml: Xml.Element
