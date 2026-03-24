package zio.blocks.docs.frontmatter.yaml

import zio.blocks.docs.Renderer as DocRenderer
import zio.blocks.schema.yaml.{Yaml, YamlWriter}

object Renderer:

  def render(doc: DocWithYamlFrontmatter): String =
    renderFrontmatter(doc) ++ renderDoc(doc)
  
  def renderFrontmatter(doc: DocWithYamlFrontmatter): String = doc.frontmatter match
    case None => ""
    case Some(map) =>
      "---\n" ++
      YamlWriter.write(Yaml.Mapping.fromStringKeys(map.toList *)) ++
      "\n---\n"
  
  def renderDoc(doc: DocWithYamlFrontmatter): String = DocRenderer.render(doc.doc)
