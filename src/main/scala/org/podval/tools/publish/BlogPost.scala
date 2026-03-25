package org.podval.tools.publish

case object BlogPost extends PageKind.Special:
  override def sourceDirectoryName: String = "_posts"

  override def is(sourcePath: Path, config: Config): Boolean =
    sourcePath.startsWith(config.blogDirectoryName)

  // TODO parse the name: year-month-day-title;
  // construct the target path: year/month/day/title.html;
  override def targetPath(sourcePath: Path): Either[String, Path] = Right(sourcePath)

  // TODO require(page.instanceOf[Page.Markup])
  // require(page.hasFrontMatter)?
  override def validate(page: Page): Either[String, Unit] = Right(())

