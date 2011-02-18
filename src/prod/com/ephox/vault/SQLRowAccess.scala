package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait SQLRowAccess[A] {
  def <|-(sql: String): RowConnector[A]
}

object SQLRowAccess {
  def sqlRowAccess[A](f: String => RowConnector[A]): SQLRowAccess[A] = new SQLRowAccess[A] {
    def <|-(sql: String) =
      f(sql)
  }

  implicit def SQLRowAccessFunctor: Functor[SQLRowAccess] = new Functor[SQLRowAccess] {
    def fmap[A, B](k: SQLRowAccess[A], f: A => B) =
      sqlRowAccess(s => k <|- s map f)
  }

  implicit def SQLRowAccessPure[M[_]]: Pure[SQLRowAccess] = new Pure[SQLRowAccess] {
    def pure[A](a: => A) =
      sqlRowAccess(_ => a.η[RowConnector])
  }

  implicit def SQLRowAccessApply[M[_]]: Apply[SQLRowAccess] = new Apply[SQLRowAccess] {
    def apply[A, B](f: SQLRowAccess[A => B], a: SQLRowAccess[A]) = {
      import RowConnector._
      sqlRowAccess(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def SQLRowAccessBind[M[_]]: Bind[SQLRowAccess] = new Bind[SQLRowAccess] {
    def bind[A, B](a: SQLRowAccess[A], f: A => SQLRowAccess[B]) =
      sqlRowAccess(s => (a <|- s) >>= (a => f(a) <|- s))
  }
}