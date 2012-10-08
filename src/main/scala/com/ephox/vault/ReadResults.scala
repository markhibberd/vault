package com.ephox.vault

import sql._, SqlT._, XSqlT._
import scalaz._, Scalaz._
import scalaz.Digit._0

sealed trait ReadResults[F[+_], +A] {
  import ReadResults._

  def impl: (ResultSet, Int) => F[(Int, XSql[A])] =
    ReadResults.impl(this)

  def map[B](f: A => B)(implicit F: Functor[F]): ReadResults[F, B] =
    ReadResultsImpl((r, n) => F.map(impl(r, n))(_ :-> (_ map f)))

  def ap[B](f: ReadResults[F, A => B])(implicit F: Bind[F]): ReadResults[F, B] =
    ReadResultsImpl((r, n) =>
      F.bind(f.impl(r, n)) {
        case (o, y) =>
          F.map(impl(r, o))(_ :-> (_ ap y ))
      })

  def flatMap[B](f: A => ReadResults[F, B])(implicit F: Monad[F]): ReadResults[F, B] =
    ReadResultsImpl((r, n) =>
      F.bind(impl(r, n)) {
        case (o, w) => {
          w.fold[F[(Int, XSql[B])]](F.point((o, XSqlT.empty[Id, B])), e => F.point((o, XSqlT.err[Id, B](e))), v => f(v).impl(r, o))
        }
      }
    )

  def orElse[AA >: A](x: => ReadResults[F, AA])(implicit F: Monad[F]): ReadResults[F, AA] =
    ReadResultsImpl((r, n) =>
      F.bind(impl(r, n)) {
        case (o, s) => s.fold[F[(Int, XSql[AA])]](F.point((o, s)), _ => F.point((o, s)), _ => x.impl(r, n))
      })

  def |||[AA >: A](x: => ReadResults[F, AA])(implicit F: Monad[F]): ReadResults[F, AA] =
    orElse(x)

  def apply(r: ResultSet)(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(impl(r, 1))(_._2.run))

  def @@>(r: ResultSet)(implicit F: Functor[F]): OptionT[F, A] =
    apply(r).toOption

  def @>(r: ResultSet)(implicit F: Functor[F]): OptionT[F, SqlError \/ A] =
    apply(r).transformer
}
private case class ReadResultsImpl[F[+_], +A](read: (ResultSet, Int) => F[(Int, XSql[A])]) extends ReadResults[F, A]

object ReadResults {
  private def impl[F[+_], A](r: ReadResults[F, A]): (ResultSet, Int) => F[(Int, XSql[A])] =
    r match {
      case ReadResultsImpl(i) => i
    }
}
