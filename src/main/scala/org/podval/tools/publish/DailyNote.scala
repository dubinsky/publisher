package org.podval.tools.publish

import org.podval.tools.publish.markdown.Markdown

object DailyNote extends PageKind.Special:
  override def sourceDirectoryName: String = "days"
    
  override def isMarkupAllowed(markup: Option[Markup]): Boolean = markup match
    case Some(Markdown) => true
    case _ => false
  
  // TODO put into the blog?
  override def targetPath(sourcePath: Path): Either[PageError, Path] = 
    Util.parseDate(sourcePath.path.last) match
      case Right(date) => Right(sourcePath)
      case Left(error) => Left(PageError(s"Daily note file name must be the date", sourcePath, Some(error)))
