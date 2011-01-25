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
      connector(c => a.η[SQLValue].η[M])
  }
}
