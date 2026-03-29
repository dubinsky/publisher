package org.podval.tools.publish

trait PageKind derives CanEqual:
  def isMarkupAllowed(markup: Option[Markup]): Boolean
  def targetPath(sourcePath: Path): Either[PageError, Path]

object PageKind:
  case object Plain extends PageKind:
    override def isMarkupAllowed(markup: Option[Markup]): Boolean = true
    override def targetPath(sourcePath: Path): Either[PageError, Path] = Right(sourcePath)

  trait Special extends PageKind:
    def sourceDirectoryName: String
    
  val special: List[Special] = List(
    BlogPost,
    DailyNote
  )

