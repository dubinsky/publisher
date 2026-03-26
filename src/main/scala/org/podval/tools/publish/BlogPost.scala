package org.podval.tools.publish

case object BlogPost extends PageKind.Special:
  override def sourceDirectoryName: String = "_posts"

  override def is(sourcePath: Path, config: Config): Boolean =
    sourcePath.startsWith(config.blogDirectoryName)

  override def targetPath(sourcePath: Path): Either[PageError, Path] =
    val fileName: String = sourcePath.path.last
    if fileName(10) != '-'
    then Left(PageError(s"Malformed blog post name: $fileName", sourcePath))
    else Util.parseDate(fileName.substring(0, 10)) match
      case Left(error) => Left(PageError(s"Blog post file name must have the date", sourcePath, Some(error)))
      case Right(date) => Right(Path(List(
        f"${date.getYear}%04d",
        f"${date.getMonthValue}%02d",
        f"${date.getDayOfMonth}%02d",
        fileName.substring(11)
      )))

  // TODO require(page.hasFrontMatter)?
  override def validate(page: Page): Either[PageError, Unit] = page match
    case page: MarkupPage => Right(())
    case _ => Left(PageError(s"Not markup", page.sourcePath))



