package com.ephox.vault

import scalaz._
import Scalaz._
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
}

object RowConnect extends RowConnects

trait RowConnects {
  def rowConnect[A](f: Connection => RowValue[A]): RowConnect[A] = new RowConnect[A] {
    val connect = f
  }

  def constantRowConnect[A](v: => RowValue[A]): RowConnect[A] =
    rowConnect(_ => v)

  def valueRowConnect[A](f: Connection => A): RowConnect[A] =
    rowConnect(f(_).η[({type λ[α]= RowValue[α]})#λ])

  def tryRowConnect[A](f: Connection => A): RowConnect[A] =
    rowConnect(c => tryRowValue(f(c)))

  def closeRowConnect: RowConnect[Unit] =
    tryRowConnect(_.close)

  implicit val RowConnectFunctor: Functor[RowConnect] = new Functor[RowConnect] {
    def fmap[A, B](k: RowConnect[A], f: A => B) =
      k map f
  }

  implicit val RowConnectPure: Pure[RowConnect] = new Pure[RowConnect] {
    def pure[A](a: => A) =
      rowConnect(_ => a.η[({type λ[α]= RowValue[α]})#λ])
  }

  implicit val RowConnectApply: Apply[RowConnect] = new Apply[RowConnect] {
    def apply[A, B](f: RowConnect[A => B], a: RowConnect[A]) = {
      rowConnect(c => {
        val fc = f(c)
        a(c) <*> fc
      })
    }
  }

  implicit val RowConnectBind: Bind[RowConnect] = new Bind[RowConnect] {
    def bind[A, B](a: RowConnect[A], f: A => RowConnect[B]) =
      rowConnect(c => a(c) >>= (a => f(a)(c)))
  }
}
