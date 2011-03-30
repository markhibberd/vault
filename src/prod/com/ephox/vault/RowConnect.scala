package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.Connection

sealed trait RowConnect[L, A] {
  val connect: Connection => RowValue[L, A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => RowConnect[L, B], k: (=> A) => RowConnect[L, C]): RowConnect[L, C] =
    this flatMap (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => RowConnect[L, B]): RowConnect[L, A] =
    rowConnect(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: RowConnect[L, A] =
    finaly(closeRowConnect)

  def map[B](f: A => B): RowConnect[L, B] =
    rowConnect(connect(_) map f)

  def flatMap[B](f: A => RowConnect[L, B]) =
    rowConnect(c => connect(c) flatMap (f(_) connect c))

  def unifyNullWithMessage(message: String): SqlConnect[L, A] =
    sqlConnect(connect(_) unifyNullWithMessage message)

  def unifyNull: SqlConnect[L, A] =
    sqlConnect(connect(_) unifyNull)

  def possiblyNull: SqlConnect[L, PossiblyNull[A]] =
    sqlConnect(connect(_) possiblyNull)

  def possiblyNullOr(d: => A): SqlConnect[L, A] =
    sqlConnect(connect(_) possiblyNullOr d)

  def |?(d: => A): SqlConnect[L, A] =
    sqlConnect(connect(_) |? d)
}

trait RowConnects {
  def rowConnect[L, A](f: Connection => RowValue[L, A]): RowConnect[L, A] = new RowConnect[L, A] {
    val connect = f
  }

  def constantRowConnect[L, A](v: => RowValue[L, A]): RowConnect[L, A] =
    rowConnect(_ => v)

  def valueRowConnect[L, A](f: Connection => A): RowConnect[L, A] =
    rowConnect(f(_).η[({type λ[α]= RowValue[L, α]})#λ])

  def tryRowConnect[L, A](f: Connection => A): RowConnect[L, A] =
    rowConnect(c => tryRowValue(f(c)))

  def closeRowConnect[L]: RowConnect[L, Unit] =
    tryRowConnect(_.close)

  implicit def RowConnectFunctor[L]: Functor[({type λ[α]= RowConnect[L, α]})#λ] = new Functor[({type λ[α]= RowConnect[L, α]})#λ] {
    def fmap[A, B](k: RowConnect[L, A], f: A => B) =
      k map f
  }

  implicit def RowConnectPure[L]: Pure[({type λ[α]= RowConnect[L, α]})#λ] = new Pure[({type λ[α]= RowConnect[L, α]})#λ] {
    def pure[A](a: => A) =
      rowConnect(_ => a.η[({type λ[α]= RowValue[L, α]})#λ])
  }

  implicit def RowConnectApply[L]: Apply[({type λ[α]= RowConnect[L, α]})#λ] = new Apply[({type λ[α]= RowConnect[L, α]})#λ] {
    def apply[A, B](f: RowConnect[L, A => B], a: RowConnect[L, A]) = {
      rowConnect(c => {
        val fc = f(c)
        a(c) <*> fc
      })
    }
  }

  implicit def RowConnectApplicative[L]: Applicative[({type λ[α]= RowConnect[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowConnect[L, α]})#λ]

  implicit def RowConnectBind[L]: Bind[({type λ[α]= RowConnect[L, α]})#λ] = new Bind[({type λ[α]= RowConnect[L, α]})#λ] {
    def bind[A, B](a: RowConnect[L, A], f: A => RowConnect[L, B]) =
      rowConnect(c => a(c) >>= (a => f(a)(c)))
  }

  implicit def RowConnectMonad[L]: Monad[({type λ[α]= RowConnect[L, α]})#λ] = Monad.monad[({type λ[α]= RowConnect[L, α]})#λ]

}
