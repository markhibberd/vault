package com.ephox.vault2

import java.sql.Connection
import scalaz._
import Scalaz._

sealed trait ConnectorM[M[_], A] {
  val connect: Connection => M[SQLValue[A]]

  def apply(c: Connection) =
    connect(c)

  def toKleisli:Kleisli[M, Connection, SQLValue[A]] =
    ☆(connect)

}

object ConnectorM {
  def connectorM[M[_], A](f: Connection => M[SQLValue[A]]): ConnectorM[M, A] = new ConnectorM[M, A] {
    val connect = f
  }

  implicit def ConnectorMFunctor[M[_]](implicit ff: Functor[M]): Functor[({type λ[α]=ConnectorM[M, α]})#λ] = new Functor[({type λ[α]=ConnectorM[M, α]})#λ] {
    def fmap[A, B](k: ConnectorM[M, A], f: A => B) =
      connectorM((c: Connection) => k(c) map (_ map f))
  }

  implicit def ConnectorMPure[M[_]](implicit pp: Pure[M]): Pure[({type λ[α]=ConnectorM[M, α]})#λ] = new Pure[({type λ[α]=ConnectorM[M, α]})#λ] {
    def pure[A](a: => A) =
      connectorM(_ => a.η[SQLValue].η[M])
  }

  implicit def ConnectorMApply[M[_]](implicit app: Apply[M], ftr: Functor[M]): Apply[({type λ[α]=ConnectorM[M, α]})#λ] = new Apply[({type λ[α]=ConnectorM[M, α]})#λ] {
    def apply[A, B](f: ConnectorM[M, A => B], a: ConnectorM[M, A]) =
      connectorM(c => f(c).<**>(a(c))(SQLValue.SQLValueApply(_, _)))
  }

  implicit def ConnectorMMonad[M[_]](implicit mnd: Monad[M]): Monad[({type λ[α]=ConnectorM[M, α]})#λ] = new Monad[({type λ[α]=ConnectorM[M, α]})#λ] {
    def bind[A, B](a: ConnectorM[M, A], f: A => ConnectorM[M, B]) =
      connectorM((c: Connection) =>
        a(c) >>= ((z: SQLValue[A]) =>
          z fold (e => SQLValue.err(e).pure[M], a => f(a).connect(c))))

    def pure[A](a: => A) = connectorM(_ => a.η[SQLValue].η[M])
  }
}
