package org.podval.tools.publish

import java.time.LocalDate
import java.time.format.DateTimeParseException

final class Relocator(
  postsDirectoryName: String,
  draftsDirectoryName: Option[String],
  dailyNotesDirectoryName: Option[String]
):
  private val dailiesMixedWithPosts: Boolean = dailyNotesDirectoryName.contains(postsDirectoryName)

  def relocate(sourcePath: Path): Either[PageError, Option[(LocalDate, Path)]] =
    val isPost: Boolean =
      sourcePath.startsWith(List(postsDirectoryName)) ||
      draftsDirectoryName.exists(draftsDirectoryName => sourcePath.startsWith(List(draftsDirectoryName)))
    val isDaily: Boolean =
      dailyNotesDirectoryName.exists(dailyNotesDirectoryName => sourcePath.startsWith(List(dailyNotesDirectoryName)))

    if !isPost && !isDaily then Right(None) else
      val fileName: String = sourcePath.fileName

      for
        date: LocalDate <-
          try
            if fileName.length < 10 then throw DateTimeParseException("Date is too short", fileName, 0)
              Right(LocalDate.parse(fileName.substring(0, 10)))
          catch case e: DateTimeParseException =>
              Left(PageError.FileName(sourcePath, s"Post and daily note names must start with date: $fileName", Some(e)))

        title: String <-
          val titleString: String = if fileName.length <= 11 then "" else fileName.substring(11).trim
          val title: String = if titleString.nonEmpty then titleString else "index"
          if dailiesMixedWithPosts then Right(title) else
            if isPost && titleString.isEmpty
            then Left(PageError.FileName(sourcePath, s"Post must have title: $fileName"))
            else if isDaily && titleString.nonEmpty
            then Left(PageError.FileName(sourcePath, s"Daily note can not have title: $fileName"))
            else Right(title)
      yield
        Some(
          date,
          Path(
            f"${date.getYear}%04d",
            f"${date.getMonthValue}%02d",
            f"${date.getDayOfMonth}%02d",
            title
          )
        )



