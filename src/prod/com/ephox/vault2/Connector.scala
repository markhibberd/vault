package com.ephox.vault2

import java.sql.Connection
import scalaz._
import Scalaz._

sealed trait Connector[M[_], A] {
  val connect: Connection => M[SQLValue[A]]

  def apply(c: Connection) =
    connect(c)
}

object Connector {
  implicit def ConnectorFunctor[M[_]](implicit ff: Functor[M]): Functor[({type λ[α]=Connector[M, α]})#λ] = new Functor[({type λ[α]=Connector[M, α]})#λ] {
    def fmap[A, B](k: Connector[M, A], f: A => B) = new Connector[M, B] {
      val connect = (c: Connection) => {
        val aa: M[SQLValue[A]] = k(c)
        val aaa = k(c) map (z => z map (y => y))
        error("") }
    }
  }
}
