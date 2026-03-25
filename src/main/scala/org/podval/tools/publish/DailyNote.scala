package org.podval.tools.publish

case object DailyNote extends PageKind.Special:
  override def sourceDirectoryName: String = "days"

  override def is(sourcePath: Path, config: Config): Boolean =
    sourcePath.startsWith(config.dailyNotesDirectoryName)

  // TODO parse the name: year-mont-day;
  // put into the blog?
  override def targetPath(sourcePath: Path): Either[String, Path] = Right(sourcePath)

  // TODO require(page.instanceOf[MarkdownPage])
  override def validate(page: Page): Either[String, Unit] = Right(())

