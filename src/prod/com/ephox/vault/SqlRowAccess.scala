package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait SqlRowAccess[L, A] {
  def <|-(sql: SqlQuery): RowConnect[L, A]

  def map[B](f: A => B): SqlRowAccess[L, B] =
    sqlRowAccess(s => this <|- s map f)

  def flatMap[B](f: A => SqlRowAccess[L, B]): SqlRowAccess[L, B] =
    sqlRowAccess(s => (this <|- s) flatMap (a => f(a) <|- s))
}

trait SqlRowAccesss {
  def sqlRowAccess[L, A](f: SqlQuery => RowConnect[L, A]): SqlRowAccess[L, A] = new SqlRowAccess[L, A] {
    def <|-(sql: SqlQuery) =
      f(sql)
  }

//  implicit def SqlRowAccessFunctor: Functor[SqlRowAccess] = new Functor[SqlRowAccess] {
//    def fmap[A, B](k: SqlRowAccess[A], f: A => B) =
//      sqlRowAccess(s => k <|- s map f)
//  }
//
//  implicit def SqlRowAccessPure[M[_]]: Pure[SqlRowAccess] = new Pure[SqlRowAccess] {
//    def pure[A](a: => A) =
//      sqlRowAccess(_ => a.η[RowConnect])
//  }
//
//  implicit def SqlRowAccessApply[M[_]]: Apply[SqlRowAccess] = new Apply[SqlRowAccess] {
//    def apply[A, B](f: SqlRowAccess[A => B], a: SqlRowAccess[A]) = {
//      sqlRowAccess(s => (a <|- s) <*> (f <|- s))
//    }
//  }
//
//  implicit def SqlRowAccessBind[M[_]]: Bind[SqlRowAccess] = new Bind[SqlRowAccess] {
//    def bind[A, B](a: SqlRowAccess[A], f: A => SqlRowAccess[B]) =
//      sqlRowAccess(s => (a <|- s) >>= (a => f(a) <|- s))
//  }
}