package com.ephox.vault

import sql._, SqlT._
import scalaz._, Scalaz._
import scalaz.Digit._0

sealed trait ReadResults[F[+_], +A] {
  import ReadResults._

  def impl: (ResultSet, Int) => F[(Int, Option[Sql[A]])] =
    ReadResults.impl(this)

  def map[B](f: A => B)(implicit F: Functor[F]): ReadResults[F, B] =
    ReadResultsImpl((r, n) => F.map(impl(r, n))(_ :-> (_ map (_ map f))))

  def ap[B](f: ReadResults[F, A => B])(implicit F: Bind[F]): ReadResults[F, B] =
    // ReadResultsImpl(F(run, f.run)((a, b) => a flatMap (x => b map (_(x)))))
    // ReadResultsImpl((r, n) => F(f.impl(r, n), impl(r, n))((a, b) => error("")))
    ReadResultsImpl((r, n) => {
      val g = F.bind(f.impl(r, n)) {
        case (o, z) => F.map(impl(r, o))(_ :-> ((aa: Option[Sql[A]]) => (ff: Option[Sql[A => B]]) => {

          error(""): Option[Sql[B]]
        }))
      }
      error("")
    })

  def flatMap[B](f: A => ReadResults[F, B])(implicit F: Monad[F]): ReadResults[F, B] =
    ReadResultsImpl((r, n) =>
      F.bind(impl(r, n)) {
        case (o, w) => w match {
          case None => F.point((o, None))
          case Some(s) => s.fold[F[(Int, Option[Sql[B]])]](
            _ => F.point((o, None))
          , a => f(a).impl(r, o)
          )
        }
      })

  def apply(r: ResultSet)(implicit F: Functor[F]): OptionT[F, Sql[A]] =
    OptionT(F.map(impl(r, 1))(_._2))
}
private case class ReadResultsImpl[F[+_], +A](read: (ResultSet, Int) => F[(Int, Option[Sql[A]])]) extends ReadResults[F, A]

object ReadResults {
  private def impl[F[+_], A](r: ReadResults[F, A]): (ResultSet, Int) => F[(Int, Option[Sql[A]])] =
    r match {
      case ReadResultsImpl(i) => i
    }
}
