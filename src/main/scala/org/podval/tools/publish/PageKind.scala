package org.podval.tools.publish

trait PageKind derives CanEqual:
  def targetPath(sourcePath: Path): Either[PageError, Path]
  def validate(page: Page): Either[PageError, Unit]

object PageKind:
  case object Plain extends PageKind:
    override def targetPath(sourcePath: Path): Either[PageError, Path] = Right(sourcePath)
    override def validate(page: Page): Either[PageError, Unit] = Right(())

  trait Special extends PageKind:
    def sourceDirectoryName: String
    def is(sourcePath: Path, config: Config): Boolean
    
  private val special: List[Special] = List(
    BlogPost,
    DailyNote
  )
  
  def get(sourcePath: Path, config: Config): PageKind =
    special.find(_.is(sourcePath, config)).getOrElse(Plain)

