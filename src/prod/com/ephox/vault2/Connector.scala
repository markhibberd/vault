package com.ephox.vault2

import java.sql.Connection
import scalaz._
import Scalaz._

sealed trait Connector[M[_], A] {
  val connect: Connection => M[SQLValue[A]]

  def apply(c: Connection) =
    connect(c)

  def toKleisli:Kleisli[M, Connection, SQLValue[A]] =
    ☆(connect)

}

object Connector {
  def connector[M[_], A](f: Connection => M[SQLValue[A]]): Connector[M, A] = new Connector[M, A] {
    val connect = f
  }

  implicit def ConnectorFunctor[M[_]](implicit ff: Functor[M]): Functor[({type λ[α]=Connector[M, α]})#λ] = new Functor[({type λ[α]=Connector[M, α]})#λ] {
    def fmap[A, B](k: Connector[M, A], f: A => B) =
      connector((c: Connection) => k(c) map (_ map f))
  }

  implicit def ConnectorPure[M[_]](implicit pp: Pure[M]): Pure[({type λ[α]=Connector[M, α]})#λ] = new Pure[({type λ[α]=Connector[M, α]})#λ] {
    def pure[A](a: => A) =
      connector(_ => a.η[SQLValue].η[M])
  }

  implicit def ConnectorApply[M[_]](implicit app: Apply[M], ftr: Functor[M]): Apply[({type λ[α]=Connector[M, α]})#λ] = new Apply[({type λ[α]=Connector[M, α]})#λ] {
    def apply[A, B](f: Connector[M, A => B], a: Connector[M, A]) =
      connector(c => f(c).<**>(a(c))(SQLValue.SQLValueApply(_, _)))
  }

  implicit def ConnectorBind[M[_]](implicit mnd: Monad[M]): Bind[({type λ[α]=Connector[M, α]})#λ] = new Bind[({type λ[α]=Connector[M, α]})#λ] {
    def bind[A, B](a: Connector[M, A], f: A => Connector[M, B]) =
      connector((c: Connection) =>
        a(c) >>= ((z: SQLValue[A]) =>
          z fold (e => SQLValue.err(e).pure[M], a => f(a).connect(c))))
  }
}
