package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait SqlRowAccess[A] {
  def <|-(sql: SQLQuery): RowConnector[A]

  def map[B](f: A => B): SqlRowAccess[B] =
    sqlRowAccess(s => this <|- s map f)

  def flatMap[B](f: A => SqlRowAccess[B]): SqlRowAccess[B] =
    sqlRowAccess(s => (this <|- s) >>= (a => f(a) <|- s))
}

trait SqlRowAccesss {
  def sqlRowAccess[A](f: SQLQuery => RowConnector[A]): SqlRowAccess[A] = new SqlRowAccess[A] {
    def <|-(sql: SQLQuery) =
      f(sql)
  }

  implicit def SqlRowAccessFunctor: Functor[SqlRowAccess] = new Functor[SqlRowAccess] {
    def fmap[A, B](k: SqlRowAccess[A], f: A => B) =
      sqlRowAccess(s => k <|- s map f)
  }

  implicit def SqlRowAccessPure[M[_]]: Pure[SqlRowAccess] = new Pure[SqlRowAccess] {
    def pure[A](a: => A) =
      sqlRowAccess(_ => a.η[RowConnector])
  }

  implicit def SqlRowAccessApply[M[_]]: Apply[SqlRowAccess] = new Apply[SqlRowAccess] {
    def apply[A, B](f: SqlRowAccess[A => B], a: SqlRowAccess[A]) = {
      sqlRowAccess(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def SqlRowAccessBind[M[_]]: Bind[SqlRowAccess] = new Bind[SqlRowAccess] {
    def bind[A, B](a: SqlRowAccess[A], f: A => SqlRowAccess[B]) =
      sqlRowAccess(s => (a <|- s) >>= (a => f(a) <|- s))
  }
}