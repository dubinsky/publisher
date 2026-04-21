package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

final class Page(
  val sourcePath: Path,
  val targetPath: Path,
  val pageKind: PageKind,
  val markup: Markup,
  val frontMatter: FrontMatter,
  var xml: Xml,
  val toc: Toc
) derives CanEqual:
  override def toString: String = s"$title.$markup($sourcePath, $targetPath)"

  def title: String = frontMatter.title.getOrElse(targetPath.path.last)

  def is(url: String): Boolean = List(
    sourcePath,
    sourcePath.withoutExtension,
    targetPath,
    targetPath.withoutExtension
  )
    .exists(_.toString.endsWith(url))

  override def equals(obj: Any): Boolean = obj match
    case that: Page => this.targetPath == that.targetPath
    case _ => false
