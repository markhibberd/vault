package com.ephox.vault2

import java.sql.Connection
import scalaz._
import Scalaz._

sealed trait Connector[A] {
  val connect: Connection => SQLValue[A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => Connector[B], k: (=> A) => Connector[C]): Connector[C] =
    this >>= (a => try { k(a) } finally { after(a) })
}

object Connector {
  import SQLValue._

  def connector[A](f: Connection => SQLValue[A]): Connector[A] = new Connector[A] {
    val connect = f
  }

  implicit def ConnectorFunctor: Functor[Connector] = new Functor[Connector] {
    def fmap[A, B](k: Connector[A], f: A => B) =
      connector((c: Connection) => k(c) map f)
  }

  implicit def ConnectorPure[M[_]]: Pure[Connector] = new Pure[Connector] {
    def pure[A](a: => A) =
      connector(_ => a.η[SQLValue])
  }

  implicit def ConnectorApply[M[_]]: Apply[Connector] = new Apply[Connector] {
    def apply[A, B](f: Connector[A => B], a: Connector[A]) =
      connector(c => a(c) <*> f(c))
  }

  implicit def ConnectorBind[M[_]]: Bind[Connector] = new Bind[Connector] {
    def bind[A, B](a: Connector[A], f: A => Connector[B]) =
      connector(c => a(c) >>= (a => f(a)(c)))
  }
}
