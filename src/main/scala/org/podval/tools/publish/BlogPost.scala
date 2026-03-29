package org.podval.tools.publish

object BlogPost extends PageKind.Special:
  override def sourceDirectoryName: String = "_posts"
  
  override def isMarkupAllowed(markup: Option[Markup]): Boolean = markup match
    case None => false
    case _ => true

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



