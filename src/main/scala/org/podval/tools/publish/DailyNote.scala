package org.podval.tools.publish

case object DailyNote extends PageKind.Special:
  override def sourceDirectoryName: String = "days"

  override def is(sourcePath: Path, config: Config): Boolean =
    sourcePath.startsWith(config.dailyNotesDirectoryName)

  // TODO put into the blog?
  override def targetPath(sourcePath: Path): Either[PageError, Path] = 
    Util.parseDate(sourcePath.path.last) match
      case Right(date) => Right(sourcePath)
      case Left(error) => Left(PageError(s"Daily note file name must be the date", sourcePath, Some(error)))
  
  // TODO require(page.instanceOf[MarkdownPage])
  override def validate(page: Page): Either[PageError, Unit] = Right(())
