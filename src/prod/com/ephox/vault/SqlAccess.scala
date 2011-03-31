package com.ephox.vault

import scalaz._, Scalaz._

sealed trait SqlAccess[L, A] {
  val access: Row => SqlValue[L, A]

  import RowAccess._

  def map[B](f: A => B): SqlAccess[L, B] = new SqlAccess[L, B] {
    val access = (r: Row) => SqlAccess.this.access(r) map f
  }

  def flatMap[B](f: A => SqlAccess[L, B]): SqlAccess[L, B] = new SqlAccess[L, B] {
    val access = (r: Row) => SqlAccess.this.access(r) flatMap (a => f(a).access(r))
  }

  def toRowAccess: RowAccess[L, A] =
    rowAccess(r => access(r).toRowValue)
}

trait SqlAccesss {
  def sqlAccess[L, A](f: Row => SqlValue[L, A]): SqlAccess[L, A] = new SqlAccess[L, A] {
    val access = f
  }

  implicit def SqlAccessFunctor[L]: Functor[({type λ[α]= SqlAccess[L, α]})#λ] = new Functor[({type λ[α]= SqlAccess[L, α]})#λ] {
    def fmap[A, B](k: SqlAccess[L, A], f: A => B) =
      k map f
  }

  implicit def SqlAccessPure[L, M[_]]: Pure[({type λ[α]= SqlAccess[L, α]})#λ] = new Pure[({type λ[α]= SqlAccess[L, α]})#λ] {
    def pure[A](a: => A) =
      sqlAccess(_ => a.η[({type λ[α]= SqlValue[L, α]})#λ])
  }

  implicit def SqlAccessApply[L, M[_]]: Apply[({type λ[α]= SqlAccess[L, α]})#λ] = new Apply[({type λ[α]= SqlAccess[L, α]})#λ] {
    def apply[A, B](f: SqlAccess[L, A => B], a: SqlAccess[L, A]) = {
      sqlAccess(r => a.access(r) <*> f.access(r))
    }
  }

  implicit def SqlAccessApplicative[L]: Applicative[({type λ[α]= SqlAccess[L, α]})#λ] = Applicative.applicative[({type λ[α]= SqlAccess[L, α]})#λ]

  implicit def SqlAccessBind[L, M[_]]: Bind[({type λ[α]= SqlAccess[L, α]})#λ] = new Bind[({type λ[α]= SqlAccess[L, α]})#λ] {
    def bind[A, B](a: SqlAccess[L, A], f: A => SqlAccess[L, B]) =
      sqlAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }

  implicit def SqlAccessMonad[L]: Monad[({type λ[α]= SqlAccess[L, α]})#λ] = Monad.monad[({type λ[α]= SqlAccess[L, α]})#λ]
}