package org.podval.tools.publish

final class Relocator(
  blogDirectoryName: String,
  dailyNotesDirectoryName: Option[String]
):
  // TODO inline
  private val locators: List[Locator] =
    List(Locator.BlogPost(blogDirectoryName)) ++
    dailyNotesDirectoryName.map(Locator.DailyNote(_)).toList

  def relocate(sourcePath: Path): Either[PageError, Path] = locators.find(_.is(sourcePath)) match
    case None => Right(sourcePath)
    case Some(locator) => locator.path(sourcePath)  
    