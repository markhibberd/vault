package com.ephox.vault2

import scalaz._
import Scalaz._
import java.sql.{ResultSet, SQLException, Connection}

sealed trait Connector[A] {
  val connect: Connection => SQLValue[A]

  import Connector._

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => Connector[B], k: (=> A) => Connector[C]): Connector[C] =
    this >>= (a => try { k(a) } finally { after(a) })

  def finaly[B](b: => Connector[B]): Connector[A] =
    connector(c => try { apply(c) } finally { b(c) })

  def finalyClose: Connector[A] =
    finaly(close)
}

object Connector {
  import SQLValue._

  def connector[A](f: Connection => SQLValue[A]): Connector[A] = new Connector[A] {
    val connect = f
  }

  def valueConnector[A](f: Connection => A): Connector[A] =
    connector(f(_).η[SQLValue])

  def tryConnector[A](f: Connection => A): Connector[A] =
    connector(c => try { value(f(c)) } catch { case e: SQLException => err(e) })

  implicit def ConnectorFunctor: Functor[Connector] = new Functor[Connector] {
    def fmap[A, B](k: Connector[A], f: A => B) =
      connector((c: Connection) => k(c) map f)
  }

  implicit def ConnectorPure[M[_]]: Pure[Connector] = new Pure[Connector] {
    def pure[A](a: => A) =
      valueConnector(_ => a)
  }

  implicit def ConnectorApply[M[_]]: Apply[Connector] = new Apply[Connector] {
    def apply[A, B](f: Connector[A => B], a: Connector[A]) =
      connector(c => a(c) <*> f(c))
  }

  implicit def ConnectorBind[M[_]]: Bind[Connector] = new Bind[Connector] {
    def bind[A, B](a: Connector[A], f: A => Connector[B]) =
      connector(c => a(c) >>= (a => f(a)(c)))
  }

  val close: Connector[Unit] =
    tryConnector(_.close)

  val executeQuery: String => Connector[ResultSet] =
    (sql: String) => tryConnector(c => {
      val s = c.createStatement
      try {
        s.executeQuery(sql)
      } finally {
        s.close
      }
  })
}
