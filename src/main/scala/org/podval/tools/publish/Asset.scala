package org.podval.tools.publish

import org.podval.tools.publish.util.Files
import org.podval.xml.Xml

sealed abstract class Asset(site: Site, path: Path) extends Page(site: Site, path: Path):
  final override def frontMatter: FrontMatter = FrontMatter.absent
  final override def resolveBlock(id: String): Option[Link.ToBlock] = None
  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] = None
  final override def resolveId(id: String): Option[Link.ToId] = None

object Asset:
  final class AssetWithSource(site: Site, path: Path) extends Asset(site, path):
    override def sourcePathOpt: Option[Path] = Some(path)
    override def write(): Unit = Files.copy(fromFile = path.file(site.sourceDirectory), toFile = targetFile)

  abstract class SyntheticAsset(site: Site, path: Path) extends Asset(site, path) with Page.WithContent:
    final override def sourcePathOpt: Option[Path] = None

  abstract class SyntheticXmlAsset(site: Site, path: Path) extends SyntheticAsset(site, path) with Page.WithContent:   
    final override def content: String = Xml.writer.render(xmlContent)
    def xmlContent: Xml.Element
  
  def syntheticAssets(site: Site): List[SyntheticAsset] = List(Sitemap(site), Robots(site), Feed(site))

  final class EmbeddedAsset(site: Site, path: Path) extends SyntheticAsset(site, path):
    override def content: String = Files.readResource(resourcesBase + path.toString)

  def embeddedAssets(site: Site): List[EmbeddedAsset] = resourcesList.map(EmbeddedAsset(site, _))

  val mainStyleSheet: String = "/assets/css/style.css"

  // TODO list using Files.listResources
  private val resourcesBase: String = "/org/podval/tools/publish/site"
  private val resourcesList: List[Path] = List(
    Path("assets", "css", "base").withExtension("css"),
    Path("assets", "css", "initialize").withExtension("css"),
    Path("assets", "css", "layout").withExtension("css"),
    Path("assets", "css", "skin").withExtension("css"),
    Path("assets", "css", "style").withExtension("css"),
  )
