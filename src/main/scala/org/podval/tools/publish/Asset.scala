package org.podval.tools.publish

abstract class Asset(
  site: Site,
  path: Path
) extends Page(
  site: Site,
  path: Path
):
  final override def sourcePathOpt: Option[Path] = None

  final override def title: String = path.fileName

  final override def resolveFragment(fragment: String): Option[Toc.Link] = None

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
