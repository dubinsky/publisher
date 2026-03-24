package zio.blocks.docs.frontmatter.yaml

import zio.blocks.docs.Doc
import zio.blocks.schema.yaml.Yaml

// Note: if my pull request https://github.com/zio/zio-blocks/pull/1262
// is accepted, this will be a part of zio-blocks, and then I will delete my copy ;)
final case class DocWithYamlFrontmatter(
  frontmatter: Option[Map[String, Yaml]],
  doc: Doc
) extends Product with Serializable:

  override def toString: String = Renderer.render(this)

  def frontmatterKey(key: String): Option[Yaml] = frontmatter.flatMap(_.get(key))
