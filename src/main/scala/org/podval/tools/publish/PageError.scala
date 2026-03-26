package org.podval.tools.publish

final class PageError(
  message: String,
  sourcePath: Path = Path.root,
  cause: Option[Throwable] = None
) extends Throwable(
  s"$message ($sourcePath)",
  cause.orNull
)
