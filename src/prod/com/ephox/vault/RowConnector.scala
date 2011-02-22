package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.Connection


sealed trait RowConnector[A] {
  val connect: Connection => RowAccess[A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => RowConnector[B], k: (=> A) => RowConnector[C]): RowConnector[C] =
    this >>= (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => RowConnector[B]): RowConnector[A] =
    RowConnector.rowConnector(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: RowConnector[A] =
    finaly(RowConnector.closeRowConnector)

  def map[B](f: A => B): RowConnector[B] =
    RowConnector.rowConnector(connect(_) map f)

  def flatMap[B](f: A => RowConnector[B]) =
    RowConnector.rowConnector(c => connect(c) flatMap (f(_) connect c))
}

object RowConnector {
  def rowConnector[A](f: Connection => RowAccess[A]): RowConnector[A] = new RowConnector[A] {
    val connect = f
  }

  def constantRowConnector[A](v: => RowAccess[A]): RowConnector[A] =
    rowConnector(_ => v)

  def valueRowConnector[A](f: Connection => A): RowConnector[A] =
    rowConnector(f(_).η[RowAccess])

  def tryRowConnector[A](f: Connection => A): RowConnector[A] =
    rowConnector(c => tryRowAccessValue(f(c)))

  val closeRowConnector: RowConnector[Unit] =
    tryRowConnector(_.close)

  implicit def RowConnectorFunctor: Functor[RowConnector] = new Functor[RowConnector] {
    def fmap[A, B](k: RowConnector[A], f: A => B) =
      rowConnector((c: Connection) => k(c) map f)
  }

  implicit def RowConnectorPure: Pure[RowConnector] = new Pure[RowConnector] {
    def pure[A](a: => A) =
      rowConnector(_ => a.η[RowAccess])
  }

  implicit def RowConnectorApply: Apply[RowConnector] = new Apply[RowConnector] {
    def apply[A, B](f: RowConnector[A => B], a: RowConnector[A]) = {
      rowConnector(c => a(c) <*> f(c))
    }
  }

  implicit def RowConnectorBind: Bind[RowConnector] = new Bind[RowConnector] {
    def bind[A, B](a: RowConnector[A], f: A => RowConnector[B]) =
      rowConnector(c => a(c) >>= (a => f(a)(c)))
  }
}
