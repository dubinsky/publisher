package org.podval.tools.publish

final class PageError(
  sourcePath: Path,
  message: String,
  cause: Option[Throwable] = None
) extends Throwable(
  s"$message ($sourcePath) ${cause.map(_.getMessage).getOrElse("")}",
  cause.orNull
)
