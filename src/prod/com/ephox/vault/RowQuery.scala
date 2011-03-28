package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowQuery[L, A] {
  def <|-(sql: Query): RowConnect[L, A]

  def map[B](f: A => B): RowQuery[L, B] =
    rowQuerys(s => this <|- s map f)

  def flatMap[B](f: A => RowQuery[L, B]): RowQuery[L, B] =
    rowQuerys(s => (this <|- s) flatMap (a => f(a) <|- s))
}

trait RowQuerys {
  def rowQuerys[L, A](f: Query => RowConnect[L, A]): RowQuery[L, A] = new RowQuery[L, A] {
    def <|-(sql: Query) =
      f(sql)
  }

  implicit def RowQueryFunctor[L]: Functor[({type λ[α]= RowQuery[L, α]})#λ] = new Functor[({type λ[α]= RowQuery[L, α]})#λ] {
    def fmap[A, B](k: RowQuery[L, A], f: A => B) =
      k map f
  }

  implicit def RowQueryPure[L, M[_]]: Pure[({type λ[α]= RowQuery[L, α]})#λ] = new Pure[({type λ[α]= RowQuery[L, α]})#λ] {
    def pure[A](a: => A) =
      rowQuerys(_ => a.η[({type λ[α]= RowConnect[L, α]})#λ])
  }

  implicit def RowQueryApply[L, M[_]]: Apply[({type λ[α]= RowQuery[L, α]})#λ] = new Apply[({type λ[α]= RowQuery[L, α]})#λ] {
    def apply[A, B](f: RowQuery[L, A => B], a: RowQuery[L, A]) = {
      rowQuerys(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def RowQueryApplicative[L]: Applicative[({type λ[α]= RowQuery[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowQuery[L, α]})#λ]

  implicit def RowQueryBind[L, M[_]]: Bind[({type λ[α]= RowQuery[L, α]})#λ] = new Bind[({type λ[α]= RowQuery[L, α]})#λ] {
    def bind[A, B](a: RowQuery[L, A], f: A => RowQuery[L, B]) =
      a flatMap f
  }

  implicit def RowQueryMonad[L]: Monad[({type λ[α]= RowQuery[L, α]})#λ] = Monad.monad[({type λ[α]= RowQuery[L, α]})#λ]
}