package org.podval.tools.publish

import java.time.LocalDate
import java.time.format.DateTimeParseException
import scala.annotation.tailrec

object Util:
  // TODO this should be available from one of the dependencies...
  def sequence[A, E, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    @tailrec
    def loop(as: List[A], result: List[B]): Either[E, List[B]] = as match
      case Nil => Right(result)
      case a :: as => f(a) match
        case Right(b) => loop(as, result :+ b)
        case Left(e) => Left(e)

    loop(as, List.empty)

  def ifDefined[A, B](
    z: => Either[PageError, Option[A]]
  )(
    c: A => Either[PageError, Option[B]]
  ): Either[PageError, Option[B]] =
    for
      opt: Option[A] <- z
      result: Option[B] <- opt match
        case None => Right(None)
        case Some(some) => c(some)
    yield
      result

  def parseDate(string: String): Either[Throwable, LocalDate] =
    try Right(LocalDate.parse(string))
    catch case e: DateTimeParseException => Left(e)
      