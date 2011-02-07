package com.ephox.vault2

import scalaz._
import Scalaz._

sealed trait RowAccessor[A] {
  val access: Row => RowAccess[A]

  def -|>[T](r: Row): IterV[A, T] => RowAccess[IterV[A, T]] =
    r -|> this
}

object RowAccessor {
  def rowAccessor[A](f: Row => RowAccess[A]): RowAccessor[A] = new RowAccessor[A] {
    val access = f
  }

  implicit def RowAccessorFunctor: Functor[RowAccessor] = new Functor[RowAccessor] {
    def fmap[A, B](k: RowAccessor[A], f: A => B) =
      rowAccessor(r => k.access(r) map f)
  }

  implicit def RowAccessorPure[M[_]]: Pure[RowAccessor] = new Pure[RowAccessor] {
    def pure[A](a: => A) =
      rowAccessor(_ => a.η[RowAccess])
  }

  implicit def RowAccessorApply[M[_]]: Apply[RowAccessor] = new Apply[RowAccessor] {
    def apply[A, B](f: RowAccessor[A => B], a: RowAccessor[A]) = {
      import RowAccess._
      rowAccessor(r => a.access(r) <*> f.access(r))
    }
  }

  implicit def RowAccessorBind[M[_]]: Bind[RowAccessor] = new Bind[RowAccessor] {
    def bind[A, B](a: RowAccessor[A], f: A => RowAccessor[B]) =
      rowAccessor(r => a.access(r) >>= (a => f(a) access (r)))
  }
}
