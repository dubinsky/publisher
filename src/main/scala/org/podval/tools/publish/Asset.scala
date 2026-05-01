package org.podval.tools.publish

abstract class Asset(
  site: Site,
  path: Path
) extends Page(
  site: Site,
  path: Path
):
  final override def title: String = path.fileName

object Asset:
  final class WithSource(
    site: Site,
    path: Path
  ) extends Asset(
    site,
    path
  ) with Page.WithSource:
    override def sourcePath: Path = path

    override def write(): Unit = Files.copy(fromFile = sourcePath.file(site.sourceDirectory), toFile = targetFile)

  abstract class Synthetic(
    site: Site,
    path: Path
  ) extends Asset(
    site, 
    path
  ) with Page.WithoutSource with Page.WithContent
  
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
