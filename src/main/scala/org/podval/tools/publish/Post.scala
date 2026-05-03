package org.podval.tools.publish

import zio.blocks.schema.xml.Xml
import java.time.LocalDate
import java.time.format.DateTimeParseException

object Post:
  // TODO
  // - turn dailies into Directories with a title override
  // - relax name restrictions
  // - detect posts and dailies by path everywhere
  // - calculate postDate from the path
  // - add FrontMatter taf "post: true" to control publishing arbitrary notes

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
  ) extends MarkupPage.Maker(site):

    private val dailiesMixedWithPosts: Boolean = dailyNotesDirectoryName.contains(postsDirectoryName)

    override def withSource(
      sourcePath: Path,
      markup: Markup,
      frontMatter: FrontMatter,
      xml: Xml.Element
    ): Option[MarkupPage] =

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
          val path: Path = Path(
            f"${date.getYear}%04d",
            f"${date.getMonthValue}%02d",
            f"${date.getDayOfMonth}%02d",
            title
          )
          MarkupPage(
            site = site,
            path = path.html,
            frontMatter = frontMatter,
            pageMarkup = Some(PageMarkup(
              sourcePath = sourcePath,
              markup = markup,
              xml = xml
            ))
          )
