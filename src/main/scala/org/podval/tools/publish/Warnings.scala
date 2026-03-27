package org.podval.tools.publish

import org.slf4j.{Logger, LoggerFactory}

final class Warnings(treatWarningsAsErrors: Boolean):
  private val log: Logger = LoggerFactory.getLogger(this.getClass)

  def recover[A](z: => Either[PageError, A])(default: => A): Either[PageError, A] = z match
    case Right(value) => Right(value)
    case Left(error) => recover(error, default)
  
  def recoverNone[A](z: => Either[PageError, A]): Either[PageError, Option[A]] = z match
    case Right(value) => Right(Some(value))
    case Left(error) => recover(error, None)
  
  private def recover[A](error: PageError, default: A): Either[PageError, A] =
    if treatWarningsAsErrors then Left(error) else
      // TODO collect all warnings with their kinds and sources
      // and generate reports for them!
      log.warn(s"Ignoring error: ${error.toString}")
      Right(default)
