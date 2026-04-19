package org.podval.tools.publish

import zio.blocks.schema.xml.Xml

// TODO extract Anchors at creation
final class Page(
  val sourcePath: Path,
  val targetPath: Path,
  val pageKind: PageKind,
  val markup: Markup,
  val frontMatter: FrontMatter,
  var xml: Xml,
) derives CanEqual:
  override def toString: String = s"Markup[$title.$markup]($sourcePath, $targetPath)"

  // TODO def is(url)
  def targetPathString: String = targetPath.withoutExtension.toString

  def fileName: String = targetPath.path.last
  def title: String = frontMatter.title.getOrElse(fileName)

  override def equals(obj: Any): Boolean = obj match
    case that: Page => this.targetPath == that.targetPath
    case _ => false
