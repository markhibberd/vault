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
}

trait RowConnects {
  def rowConnect[L, A](f: Connection => RowValue[L, A]): RowConnect[L, A] = new RowConnect[L, A] {
    val connect = f
  }

  def constantRowConnect[L, A](v: => RowValue[L, A]): RowConnect[L, A] =
    rowConnect(_ => v)

  def valueRowConnect[L, A](f: Connection => A): RowConnect[L, A] =
    // rowConnect(f(_).η[RowValue])
    error("todo")

  def tryRowConnect[L, A](f: Connection => A): RowConnect[L, A] =
    rowConnect(c => tryRowValue(f(c)))

  def closeRowConnect[L]: RowConnect[L, Unit] =
    tryRowConnect(_.close)
//
//  implicit def RowConnectFunctor: Functor[RowConnect] = new Functor[RowConnect] {
//    def fmap[A, B](k: RowConnect[A], f: A => B) =
//      rowConnect((c: Connection) => k(c) map f)
//  }
//
//  implicit def RowConnectPure: Pure[RowConnect] = new Pure[RowConnect] {
//    def pure[A](a: => A) =
//      rowConnect(_ => a.η[RowValue])
//  }
//
//  implicit def RowConnectApply: Apply[RowConnect] = new Apply[RowConnect] {
//    def apply[A, B](f: RowConnect[A => B], a: RowConnect[A]) = {
//      rowConnect(c => a(c) <*> f(c))
//    }
//  }
//
//  implicit def RowConnectBind: Bind[RowConnect] = new Bind[RowConnect] {
//    def bind[A, B](a: RowConnect[A], f: A => RowConnect[B]) =
//      rowConnect(c => a(c) >>= (a => f(a)(c)))
//  }
}
