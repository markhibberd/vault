package com.ephox.vault

import java.sql.Connection
import scalaz._
import Scalaz._

sealed trait ConnectorT[M[_], A] {
  val connect: Connection => M[SQLValue[A]]

  def apply(c: Connection) =
    connect(c)

  def toKleisli:Kleisli[M, Connection, SQLValue[A]] =
    ☆(connect)

}

object ConnectorT {
  def connectorT[M[_], A](f: Connection => M[SQLValue[A]]): ConnectorT[M, A] = new ConnectorT[M, A] {
    val connect = f
  }

  implicit def ConnectorTFunctor[M[_]](implicit ff: Functor[M]): Functor[({type λ[α]=ConnectorT[M, α]})#λ] = new Functor[({type λ[α]=ConnectorT[M, α]})#λ] {
    def fmap[A, B](k: ConnectorT[M, A], f: A => B) =
      connectorT((c: Connection) => k(c) map (_ map f))
  }

  implicit def ConnectorTPure[M[_]](implicit pp: Pure[M]): Pure[({type λ[α]=ConnectorT[M, α]})#λ] = new Pure[({type λ[α]=ConnectorT[M, α]})#λ] {
    def pure[A](a: => A) =
      connectorT(_ => a.η[SQLValue].η[M])
  }

  implicit def ConnectorTApply[M[_]](implicit app: Apply[M], ftr: Functor[M]): Apply[({type λ[α]=ConnectorT[M, α]})#λ] = new Apply[({type λ[α]=ConnectorT[M, α]})#λ] {
    def apply[A, B](f: ConnectorT[M, A => B], a: ConnectorT[M, A]) =
      connectorT(c => f(c).<**>(a(c))(SQLValueApply(_, _)))
  }

  implicit def ConnectorTMonad[M[_]](implicit mnd: Monad[M]): Monad[({type λ[α]=ConnectorT[M, α]})#λ] = new Monad[({type λ[α]=ConnectorT[M, α]})#λ] {
    def bind[A, B](a: ConnectorT[M, A], f: A => ConnectorT[M, B]) =
      connectorT((c: Connection) =>
        a(c) >>= ((z: SQLValue[A]) =>
          z fold (e => sqlError(e).pure[M], a => f(a).connect(c))))

    def pure[A](a: => A) = connectorT(_ => a.η[SQLValue].η[M])
  }
}
