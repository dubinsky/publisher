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

  final override def resolveBlock(id: String): Option[Link.ToBlock] = None

  final override def resolveSection(names: Seq[String]): Option[Link.ToSection] = None

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
