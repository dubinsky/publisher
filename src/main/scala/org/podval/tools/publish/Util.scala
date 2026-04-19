package org.podval.tools.publish

import zio.blocks.chunk.Chunk
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

  def sequence[A, E, B](as: Chunk[A])(f: A => Either[E, B]): Either[E, Chunk[B]] =
    @tailrec
    def loop(as: Chunk[A], result: Chunk[B]): Either[E, Chunk[B]] =
      if as.isEmpty then Right(result) else f(as.head) match
        case Right(b) => loop(as.tail, result :+ b)
        case Left(e) => Left(e)

    loop(as, Chunk.empty)

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
