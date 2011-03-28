package com.ephox.vault

import scalaz._, Scalaz._

sealed trait SqlQueryConnect[L, A] {
  def <|-(sql: Query): SqlConnect[L, A]

  def map[B](f: A => B): SqlQueryConnect[L, B] =
    sqlQueryConnect(s => this <|- s map f)

  def flatMap[B](f: A => SqlQueryConnect[L, B]): SqlQueryConnect[L, B] =
    sqlQueryConnect(s => (this <|- s) flatMap (a => f(a) <|- s))
}

trait SqlQueryConnects {
  def sqlQueryConnect[L, A](f: Query => SqlConnect[L, A]): SqlQueryConnect[L, A] = new SqlQueryConnect[L, A] {
    def <|-(sql: Query) =
      f(sql)
  }

  implicit def SqlQueryConnectFunctor[L]: Functor[({type λ[α]= SqlQueryConnect[L, α]})#λ] = new Functor[({type λ[α]= SqlQueryConnect[L, α]})#λ] {
    def fmap[A, B](k: SqlQueryConnect[L, A], f: A => B) =
      k map f
  }

  implicit def SqlQueryConnectPure[L, M[_]]: Pure[({type λ[α]= SqlQueryConnect[L, α]})#λ] = new Pure[({type λ[α]= SqlQueryConnect[L, α]})#λ] {
    def pure[A](a: => A) =
      sqlQueryConnect(_ => a.η[({type λ[α]= SqlConnect[L, α]})#λ])
  }

  implicit def SqlQueryConnectApply[L, M[_]]: Apply[({type λ[α]= SqlQueryConnect[L, α]})#λ] = new Apply[({type λ[α]= SqlQueryConnect[L, α]})#λ] {
    def apply[A, B](f: SqlQueryConnect[L, A => B], a: SqlQueryConnect[L, A]) = {
      sqlQueryConnect(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def SqlQueryConnectApplicative[L]: Applicative[({type λ[α]= SqlQueryConnect[L, α]})#λ] = Applicative.applicative[({type λ[α]= SqlQueryConnect[L, α]})#λ]

  implicit def SqlQueryConnectBind[L, M[_]]: Bind[({type λ[α]= SqlQueryConnect[L, α]})#λ] = new Bind[({type λ[α]= SqlQueryConnect[L, α]})#λ] {
    def bind[A, B](a: SqlQueryConnect[L, A], f: A => SqlQueryConnect[L, B]) =
      a flatMap f
  }

  implicit def SqlQueryConnectMonad[L]: Monad[({type λ[α]= SqlQueryConnect[L, α]})#λ] = Monad.monad[({type λ[α]= SqlQueryConnect[L, α]})#λ]
}
