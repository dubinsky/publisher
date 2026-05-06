package org.podval.tools.publish

import java.time.LocalDate
import java.time.format.DateTimeParseException

object Post:
  def date(path: Path): Option[LocalDate] =
    if path.path.length != 4 then None else
      val dateString = s"${path.path(0)}-${path.path(1)}-${path.path(2)}"
      try Some(LocalDate.parse(dateString))
      catch case e: DateTimeParseException => None

  final class Maker(
    site: Site,
    postsDirectoryName: String,
    draftsDirectoryName: Option[String],
    dailyNotesDirectoryName: Option[String]
  ) extends MarkupPage.Maker[MarkupPage](MarkupPage.apply):
    private val dailiesMixedWithPosts: Boolean = dailyNotesDirectoryName.contains(postsDirectoryName)

    override def path(sourcePath: Path): Option[Path] =
      val isPost: Boolean =
        sourcePath.startsWith(List(postsDirectoryName)) ||
          draftsDirectoryName.exists(draftsDirectoryName => sourcePath.startsWith(List(draftsDirectoryName)))

      val isDaily: Boolean =
        dailyNotesDirectoryName.exists(dailyNotesDirectoryName => sourcePath.startsWith(List(dailyNotesDirectoryName)))

      if !isPost && !isDaily then None else
        val fileName: String = sourcePath.fileName

        for
          date: LocalDate <-
            try
              if fileName.length < 10 then throw DateTimeParseException("Date is too short", fileName, 0)
              Some(LocalDate.parse(fileName.substring(0, 10)))
            catch case e: DateTimeParseException =>
              site.reportError(
                PageError.FileName(sourcePath, s"Post and daily note names must start with date: $fileName", Some(e)),
                None
              )

          title: String <-
            val titleString: String = if fileName.length <= 11 then "" else fileName.substring(11).trim
            val title: String = if titleString.nonEmpty then titleString else Directory.fileName
            if dailiesMixedWithPosts then Some(title) else if isPost && titleString.isEmpty
            then site.reportError(PageError.FileName(sourcePath, s"Post must have title: $fileName"), None)
            else if isDaily && titleString.nonEmpty
            then site.reportError(PageError.FileName(sourcePath, s"Daily note can not have title: $fileName"), None)
            else Some(title)
        yield
          Path(
            f"${date.getYear}%04d",
            f"${date.getMonthValue}%02d",
            f"${date.getDayOfMonth}%02d",
            title
          )
