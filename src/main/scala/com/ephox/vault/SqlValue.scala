package com.ephox
package vault

import SqlValue._
import scalaz._, Scalaz._, Validation._

sealed trait SqlValue[+L, +A] {
  val log: Log[L]
  val value: SqlExceptionContext \/ A

  final def loop[LL >: L, AA >: A, X](e: SqlExceptionContext => X, v: AA => X \/ SqlValue[LL, AA]): X =
    value.loopr(e, (a: AA) => v(a) map (_.value))

  final def loope[LL >: L, AA >: A, X](e: SqlExceptionContext => X \/ SqlValue[LL, AA], v: AA => X): X =
    value.loopl((c: SqlExceptionContext) => e(c) map (_.value), v)

  def isError: Boolean =
    value.isLeft

  def isValue: Boolean =
    value.isRight

  def toValidation: SqlExceptionContext \?/ A =
    value.validation

  def map[B](f: A => B): SqlValue[L, B] =
    SqlValue(log, value map f)

  def flatMap[LL >: L, B](f: A => SqlValue[LL, B])(implicit S: Semigroup[LL]): SqlValue[LL, B] =
    value.fold(
      e => SqlValue(log, e.left)
    , a => sqlValueLogL[LL, B].mod(log ++ _, f(a))
    )

  def foreach(f: A => Unit): Unit =
    value foreach f

  def forall(p: A => Boolean): Boolean =
    value forall p

  def exists(p: A => Boolean): Boolean =
    value exists p

  def ap[LL >: L, B](f: => SqlValue[LL, A => B])(implicit S: Semigroup[LL]): SqlValue[LL, B] =
    f flatMap (ff => map(ff(_)))

  def traverse[F[+_]: Applicative, B](g: A => F[B]): F[SqlValue[L, B]] =
    value.fold(
      e => Applicative[F].point(SqlValue(log, e.left))
    , a => Functor[F].map(g(a))(b => SqlValue(log, b.right))
    )

  def bimap[M, B](f: L => M, g: A => B): SqlValue[M, B] =
    SqlValue(log map f, value map g)

  def bitraverse[F[+_]: Applicative, M, B](f: L => F[M], g: A => F[B]): F[SqlValue[M, B]] =
    Applicative[F].ap(Functor[F].map(Traverse[List].traverse(log.toList)(f))(x => Vector(x: _*)))(
      value.fold(
        e => Applicative[F].point(SqlValue(_, e.left))
      , a => Functor[F].map(g(a))(b => (m: Log[M]) => SqlValue(m, b.right))
      )
    )

  def toList: List[A] =
    value.toList

  def toStream: Stream[A] =
    value.toStream

  def toOption: Option[A] =
    value.toOption

  def orElse[LL >: L, AA >: A](x: => SqlValue[LL, AA]): SqlValue[LL, AA] =
    value.fold(
      e => x
    , a => SqlValue(log, a.right)
    )

  def |||[LL >: L, AA >: A](x: => SqlValue[LL, AA]): SqlValue[LL, AA] =
    orElse(x)

}

object SqlValue extends SqlValueFunctions {
  def apply[L, A](l: Log[L], v: SqlExceptionContext \/ A): SqlValue[L, A] =
    new SqlValue[L, A] {
      val log = l
      val value = v
    }
}

trait SqlValueFunctions {
  type Log[+L] =
  Vector[L]

  def sqlValueLogL[L, A]: SqlValue[L, A] @> Log[L] =
    Lens(v => Store(SqlValue(_, v.value), v.log))

  def sqlValueOrL[L, A]: SqlValue[L, A] @> (SqlExceptionContext \/ A) =
    Lens(v => Store(SqlValue(v.log, _), v.value))

  def sqlValueContextPL[L, A]: SqlValue[L, A] @?> SqlExceptionContext =
    ~sqlValueOrL >=> PLensT.leftPLens

  def sqlValuePL[L, A]: SqlValue[L, A] @?> A =
    ~sqlValueOrL >=> PLensT.rightPLens
}
