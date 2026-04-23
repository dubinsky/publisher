package org.podval.tools.publish

import java.time.LocalDate
import java.time.format.DateTimeParseException

trait PageKind derives CanEqual:
  def isAssetAllowed: Boolean
  def isMarkupAllowed(markup: Markup): Boolean
  // TODO isMarkupRequired: Boolean
  def targetPath(sourcePath: Path): Either[PageError, Path]

object PageKind:
  case object Plain extends PageKind:
    override def isAssetAllowed: Boolean = true
    override def isMarkupAllowed(markup: Markup): Boolean = true
    override def targetPath(sourcePath: Path): Either[PageError, Path] = Right(sourcePath)

  trait Special extends PageKind:
    def sourceDirectoryName: String

  object BlogPost extends Special:
    override def sourceDirectoryName: String = "_posts"

    override def isAssetAllowed: Boolean = false

    override def isMarkupAllowed(markup: Markup): Boolean = true

    override def targetPath(sourcePath: Path): Either[PageError, Path] =
      val fileName: String = sourcePath.path.last
      if fileName(10) != '-'
      then PageError.FileName(sourcePath, s"Malformed blog post name: $fileName")
      else parseDate(fileName.substring(0, 10)) match
        case Left(error) => PageError.FileName(sourcePath, s"Blog post file name must have the date", Some(error))
        case Right(date) => Right(Path(List(
          f"${date.getYear}%04d",
          f"${date.getMonthValue}%02d",
          f"${date.getDayOfMonth}%02d",
          fileName.substring(11)
        )))

  object DailyNote extends Special:
    override def sourceDirectoryName: String = "days"

    override def isAssetAllowed: Boolean = false

    override def isMarkupAllowed(markup: Markup): Boolean = markup match
      case Markdown => true
      case _ => false

    // TODO put into the blog?
    override def targetPath(sourcePath: Path): Either[PageError, Path] =
      parseDate(sourcePath.path.last) match
        case Right(date) => Right(sourcePath)
        case Left(error) => PageError.FileName(sourcePath, s"Daily note file name must be the date", Some(error))

  private def parseDate(string: String): Either[Throwable, LocalDate] =
    try Right(LocalDate.parse(string))
    catch case e: DateTimeParseException => Left(e)

  private val special: List[Special] = List(
    BlogPost,
    DailyNote
  )

  // TODO move into Config
  def apply(sourcePath: Path, config: Config): PageKind = PageKind.special
    .find(special => sourcePath.startsWith(config.specialPageKindSourcePathStartsWith(special)))
    .getOrElse(PageKind.Plain)
