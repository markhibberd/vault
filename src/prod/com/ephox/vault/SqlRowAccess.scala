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

object SqlRowAccess {
  implicit def SqlRowAccessFunctor[L]: Functor[({type λ[α]= SqlRowAccess[L, α]})#λ] = new Functor[({type λ[α]= SqlRowAccess[L, α]})#λ] {
    def fmap[A, B](k: SqlRowAccess[L, A], f: A => B) =
      k map f
  }

  implicit def SqlRowAccessPure[L, M[_]]: Pure[({type λ[α]= SqlRowAccess[L, α]})#λ] = new Pure[({type λ[α]= SqlRowAccess[L, α]})#λ] {
    def pure[A](a: => A) =
      sqlRowAccess(_ => a.η[({type λ[α]= RowConnect[L, α]})#λ])
  }

  implicit def SqlRowAccessApply[L, M[_]]: Apply[({type λ[α]= SqlRowAccess[L, α]})#λ] = new Apply[({type λ[α]= SqlRowAccess[L, α]})#λ] {
    def apply[A, B](f: SqlRowAccess[L, A => B], a: SqlRowAccess[L, A]) = {
      sqlRowAccess(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def SqlRowAccessApplicative[L]: Applicative[({type λ[α]= SqlRowAccess[L, α]})#λ] = Applicative.applicative[({type λ[α]= SqlRowAccess[L, α]})#λ]

  implicit def SqlRowAccessBind[L, M[_]]: Bind[({type λ[α]= SqlRowAccess[L, α]})#λ] = new Bind[({type λ[α]= SqlRowAccess[L, α]})#λ] {
    def bind[A, B](a: SqlRowAccess[L, A], f: A => SqlRowAccess[L, B]) =
      a flatMap f
  }

  implicit def SqlRowAccessMonad[L]: Monad[({type λ[α]= SqlRowAccess[L, α]})#λ] = Monad.monad[({type λ[α]= SqlRowAccess[L, α]})#λ]
}
trait SqlRowAccesss {
  def sqlRowAccess[L, A](f: SqlQuery => RowConnect[L, A]): SqlRowAccess[L, A] = new SqlRowAccess[L, A] {
    def <|-(sql: SqlQuery) =
      f(sql)
  }
}