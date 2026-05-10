package org.podval.tools.publish

final class PageError(
  kind: PageError.Kind,
  sourcePath: Path,
  message: String,
  cause: Option[Throwable]
) extends Throwable(
  s"$kind: $message ($sourcePath) ${cause.map(_.getMessage).getOrElse("")}",
  cause.orNull
):
  def report[R](site: Site, result: R): R = site.reportError(this, result)
  def report[R](site: Site): Option[R] = site.reportError(this, None)

object PageError:
  sealed abstract class Kind(override val toString: String):
    final def apply(
      sourcePath: Path,
      message: String,
      cause: Option[Throwable] = None
    ): PageError = PageError(
      this,
      sourcePath,
      message,
      cause
    )

  case object Parsing extends Kind("parsing")
  case object FileName extends Kind("file name")
  case object FileKind extends Kind("file kind")
  case object Duplicate extends Kind("duplicate")
  case object Unmakable extends Kind("unmakable")
  case object NoId extends Kind("no id")
  case object Unresolved extends Kind("unresolved")

