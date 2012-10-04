package com.ephox.vault

import sql._, SqlT._
import scalaz._, Scalaz._

sealed trait ReadResults[F[+_], +A] {
  import ReadResults._

  def apply(r: ResultSet)(implicit F: Functor[F]): OptionT[F, Sql[A]] =
    OptionT(F.map(impl(this)(r, 1))(_._2))
}
private case class ReadResultsImpl[F[+_], +A](read: (ResultSet, Int) => F[(Int, Option[Sql[A]])]) extends ReadResults[F, A]

object ReadResults {
  private def impl[F[+_], A](r: ReadResults[F, A]): (ResultSet, Int) => F[(Int, Option[Sql[A]])] =
    r match {
      case ReadResultsImpl(i) => i
    }
}
