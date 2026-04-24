package org.podval.tools.publish

import java.time.LocalDate
import java.time.format.DateTimeParseException

sealed abstract class Locator(sourceDirectoryName: String) derives CanEqual:
  def is(sourcePath: Path): Boolean = sourcePath.startsWith(sourceDirectoryName)
  def isAssetAllowed: Boolean
  def isMarkupAllowed(markup: Markup): Boolean
  // TODO isMarkupRequired: Boolean
  def targetPath(sourcePath: Path): Either[PageError, Path]

object Locator:
  object BlogPost:
    def sourceDirectoryNameDefault: String = "_posts"

  final class BlogPost(sourceDirectoryName: String) extends Locator(sourceDirectoryName):
    override def isAssetAllowed: Boolean = false

    override def isMarkupAllowed(markup: Markup): Boolean = true

    override def targetPath(sourcePath: Path): Either[PageError, Path] =
      val fileName: String = sourcePath.path.last
      if fileName(10) != '-'
      then Left(PageError.FileName(sourcePath, s"Malformed blog post name: $fileName"))
      else parseDate(fileName.substring(0, 10)) match
        case Left(error) => Left(PageError.FileName(sourcePath, s"Blog post file name must have the date", Some(error)))
        case Right(date) => Right(Path(List(
          f"${date.getYear}%04d",
          f"${date.getMonthValue}%02d",
          f"${date.getDayOfMonth}%02d",
          fileName.substring(11)
        )))
  
  final class DailyNote(sourceDirectoryName: String) extends Locator(sourceDirectoryName):
    override def isAssetAllowed: Boolean = false

    override def isMarkupAllowed(markup: Markup): Boolean = markup match
      case Markdown => true
      case _ => false

    // TODO put into the blog!
    override def targetPath(sourcePath: Path): Either[PageError, Path] =
      parseDate(sourcePath.path.last) match
        case Right(date) => Right(sourcePath)
        case Left(error) => Left(PageError.FileName(sourcePath, s"Daily note file name must be the date", Some(error)))

  private def parseDate(string: String): Either[Throwable, LocalDate] =
    try Right(LocalDate.parse(string))
    catch case e: DateTimeParseException => Left(e)
