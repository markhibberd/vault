package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.Connection

sealed trait RowConnect[A] {
  val connect: Connection => RowValue[A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => RowConnect[B], k: (=> A) => RowConnect[C]): RowConnect[C] =
    this >>= (a => try {
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
}

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

  val closeRowConnect: RowConnect[Unit] =
    tryRowConnect(_.close)

  implicit def RowConnectFunctor: Functor[RowConnect] = new Functor[RowConnect] {
    def fmap[A, B](k: RowConnect[A], f: A => B) =
      rowConnect((c: Connection) => k(c) map f)
  }

  implicit def RowConnectPure: Pure[RowConnect] = new Pure[RowConnect] {
    def pure[A](a: => A) =
      rowConnect(_ => a.η[RowValue])
  }

  implicit def RowConnectApply: Apply[RowConnect] = new Apply[RowConnect] {
    def apply[A, B](f: RowConnect[A => B], a: RowConnect[A]) = {
      rowConnect(c => a(c) <*> f(c))
    }
  }

  implicit def RowConnectBind: Bind[RowConnect] = new Bind[RowConnect] {
    def bind[A, B](a: RowConnect[A], f: A => RowConnect[B]) =
      rowConnect(c => a(c) >>= (a => f(a)(c)))
  }
}
