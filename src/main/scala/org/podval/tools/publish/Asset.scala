package org.podval.tools.publish

abstract class Asset(
  site: Site,
  path: Path
) extends Page(
  site: Site,
  path: Path
):
  final override def sourcePathOpt: Option[Path] = None

  final override def title: String = path.fileNameWithNonHtmlExtension

  final override def aliases: List[String] = List.empty

  final override def icon: FontAwesome.Icon = FontAwesome.file
  
  final override def resolveBlock(id: String): Option[Link.ToBlock] = None

  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] = None

  final override def resolveId(id: String): Option[Link.ToId] = None

object Asset:
  final class WithSource(
    site: Site,
    path: Path
  ) extends Asset(
    site,
    path
  ):
    override def write(): Unit = Files.copy(fromFile = path.file(site.sourceDirectory), toFile = targetFile)

  abstract class Synthetic(
    site: Site,
    path: Path
  ) extends Asset(
    site, 
    path
  ) with Page.WithContent
  
  final class Embedded(
    site: Site,
    path: Path
  ) extends Synthetic(
    site,
    path
  ):
    override def content: String = Files.readResource(Site.resourcesBase + path.toString)
  
  abstract class SyntheticXml(
    site: Site,
    path: Path
  ) extends Synthetic(
    site,
    path
  ) with Page.WithXmlContent
