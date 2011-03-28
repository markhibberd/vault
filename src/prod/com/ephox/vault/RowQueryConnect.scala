package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowQueryConnect[L, A] {
  def <|-(sql: Query): RowConnect[L, A]

  def map[B](f: A => B): RowQueryConnect[L, B] =
    rowQueryConnect(s => this <|- s map f)

  def flatMap[B](f: A => RowQueryConnect[L, B]): RowQueryConnect[L, B] =
    rowQueryConnect(s => (this <|- s) flatMap (a => f(a) <|- s))
}

trait RowQueryConnects {
  def rowQueryConnect[L, A](f: Query => RowConnect[L, A]): RowQueryConnect[L, A] = new RowQueryConnect[L, A] {
    def <|-(sql: Query) =
      f(sql)
  }

  implicit def RowQueryConnectFunctor[L]: Functor[({type λ[α]= RowQueryConnect[L, α]})#λ] = new Functor[({type λ[α]= RowQueryConnect[L, α]})#λ] {
    def fmap[A, B](k: RowQueryConnect[L, A], f: A => B) =
      k map f
  }

  implicit def RowQueryConnectPure[L, M[_]]: Pure[({type λ[α]= RowQueryConnect[L, α]})#λ] = new Pure[({type λ[α]= RowQueryConnect[L, α]})#λ] {
    def pure[A](a: => A) =
      rowQueryConnect(_ => a.η[({type λ[α]= RowConnect[L, α]})#λ])
  }

  implicit def RowQueryConnectApply[L, M[_]]: Apply[({type λ[α]= RowQueryConnect[L, α]})#λ] = new Apply[({type λ[α]= RowQueryConnect[L, α]})#λ] {
    def apply[A, B](f: RowQueryConnect[L, A => B], a: RowQueryConnect[L, A]) = {
      rowQueryConnect(s => (a <|- s) <*> (f <|- s))
    }
  }

  implicit def RowQueryConnectApplicative[L]: Applicative[({type λ[α]= RowQueryConnect[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowQueryConnect[L, α]})#λ]

  implicit def RowQueryConnectBind[L, M[_]]: Bind[({type λ[α]= RowQueryConnect[L, α]})#λ] = new Bind[({type λ[α]= RowQueryConnect[L, α]})#λ] {
    def bind[A, B](a: RowQueryConnect[L, A], f: A => RowQueryConnect[L, B]) =
      a flatMap f
  }

  implicit def RowQueryConnectMonad[L]: Monad[({type λ[α]= RowQueryConnect[L, α]})#λ] = Monad.monad[({type λ[α]= RowQueryConnect[L, α]})#λ]
}