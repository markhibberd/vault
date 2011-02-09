package com.ephox.vault2

import scalaz._
import Scalaz._
import java.sql.Connection


sealed trait RowConnector[A] {
  val connect: Connection => RowAccess[A]

  def apply(c: Connection) = connect(c)

}

object RowConnector {
  def rowConnector[A](f: Connection => RowAccess[A]): RowConnector[A] = new RowConnector[A] {
    val connect = f
  }

  implicit def RowConnectorFunctor: Functor[RowConnector] = new Functor[RowConnector] {
    def fmap[A, B](k: RowConnector[A], f: A => B) =
      rowConnector((c: Connection) => k(c) map f)
  }

  implicit def RowConnectorPure[M[_]]: Pure[RowConnector] = new Pure[RowConnector] {
    def pure[A](a: => A) =
      rowConnector(_ => a.η[RowAccess])
  }

  implicit def RowConnectorApply[M[_]]: Apply[RowConnector] = new Apply[RowConnector] {
    def apply[A, B](f: RowConnector[A => B], a: RowConnector[A]) = {
      import RowAccess._
      rowConnector(c => a(c) <*> f(c))
    }
  }

  implicit def RowConnectorBind[M[_]]: Bind[RowConnector] = new Bind[RowConnector] {
    def bind[A, B](a: RowConnector[A], f: A => RowConnector[B]) =
      rowConnector(c => a(c) >>= (a => f(a)(c)))
  }
}
