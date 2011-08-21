package com.ephox.vault

import scalaz._, Scalaz._
import RowValue._
import java.sql.Connection

sealed trait RowConnect[A] {
  val connect: Connection => RowValue[A]

  import SqlConnect._
  import RowConnect._

  def apply(c: Connection) = connect(c)

  def executeOrDie(c: Connection) = connect(c).getOrDie

  def bracket[B, C](after: (=> A) => RowConnect[B], k: (=> A) => RowConnect[C]): RowConnect[C] =
    this flatMap (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => RowConnect[B]): RowConnect[A] =
    rowConnect(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: RowConnect[A] =
    finaly(closeRowConnect)

  def map[B](f: A => B): RowConnect[B] =
    rowConnect(connect(_) map f)

  def flatMap[B](f: A => RowConnect[B]) =
    rowConnect(c => connect(c) flatMap (f(_) connect c))

  def unifyNullWithMessage(message: String): SqlConnect[A] =
    sqlConnect(connect(_) unifyNullWithMessage message)

  def unifyNull: SqlConnect[A] =
    sqlConnect(connect(_) unifyNull)

  def possiblyNull: SqlConnect[PossiblyNull[A]] =
    sqlConnect(connect(_) possiblyNull)

  def possiblyNullOr(d: => A): SqlConnect[A] =
    sqlConnect(connect(_) possiblyNullOr d)

  def |?(d: => A): SqlConnect[A] =
    sqlConnect(connect(_) |? d)

  /**
   * Lifts this value into a possibly null value. The following holds:
   *
   * forall r c. r.liftPossiblyNull.connect(c).isNotNull
   */
  def liftPossiblyNull: RowConnect[PossiblyNull[A]] =
    possiblyNull.toRowConnect

  def toKleisli: Kleisli[RowValue, Connection, A] =
    kleisli(connect)
}

object RowConnect extends RowConnects

trait RowConnects {
  def rowConnect[A](f: Connection => RowValue[A]): RowConnect[A] = new RowConnect[A] {
    val connect = f
  }

  def constantRowConnect[A](v: => RowValue[A]): RowConnect[A] =
    rowConnect(_ => v)

  def valueRowConnect[A](f: Connection => A): RowConnect[A] =
    rowConnect(f(_).η[RowValue])

  def tryRowConnect[A](f: Connection => A): RowConnect[A] =
    rowConnect(c => tryRowValue(f(c)))

  def closeRowConnect: RowConnect[Unit] =
    tryRowConnect(_.close)

  def kleisliRowConnect[A](k: Kleisli[RowValue, Connection, A]): RowConnect[A] =
    rowConnect(k(_))

  def foldTraverseRowConnect[T[_]: Foldable, A, B](w: T[A], g: A => RowConnect[B]): RowConnect[List[B]] =
    kleisliRowConnect(w.listl.reverse.traverseKleisli[Connection, RowValue, B](a => g(a).toKleisli)) map (_.reverse)

  implicit val RowConnectFunctor: Functor[RowConnect] = new Functor[RowConnect] {
    def fmap[A, B](k: RowConnect[A], f: A => B) =
      k map f
  }

  implicit val RowConnectPure: Pure[RowConnect] = new Pure[RowConnect] {
    def pure[A](a: => A) =
      rowConnect(_ => a.η[RowValue])
  }

  implicit val RowConnectBind: Bind[RowConnect] = new Bind[RowConnect] {
    def bind[A, B](a: RowConnect[A], f: A => RowConnect[B]) =
      rowConnect(c => a(c) >>= (a => f(a)(c)))
  }

  /** WARNING: Unsafe function */
  def rtrace[A](a: A)(implicit s: Show[A]): RowConnect[Unit] =
    rowConnect(_ => {
      a.println
      ().pure[RowValue]
    })

}
