package com.ephox.vault

import scalaz._, Scalaz._

sealed trait SqlAccess[A] {
  val access: Row => SqlValue[A]

  import RowAccess._

  def map[B](f: A => B): SqlAccess[B] = new SqlAccess[B] {
    val access = (r: Row) => SqlAccess.this.access(r) map f
  }

  def flatMap[B](f: A => SqlAccess[B]): SqlAccess[B] = new SqlAccess[B] {
    val access = (r: Row) => SqlAccess.this.access(r) flatMap (a => f(a).access(r))
  }

  def toRowAccess: RowAccess[A] =
    rowAccess(r => access(r).toRowValue)
}

object SqlAccess extends SqlAccesss

trait SqlAccesss {
  def sqlAccess[A](f: Row => SqlValue[A]): SqlAccess[A] = new SqlAccess[A] {
    val access = f
  }

  implicit val SqlAccessFunctor: Functor[SqlAccess] = new Functor[SqlAccess] {
    def fmap[A, B](k: SqlAccess[A], f: A => B) =
      k map f
  }

  implicit val SqlAccessPure: Pure[SqlAccess] = new Pure[SqlAccess] {
    def pure[A](a: => A) =
      sqlAccess(_ => a.η[SqlValue])
  }

  implicit val SqlAccessApply: Apply[SqlAccess] = new Apply[SqlAccess] {
    def apply[A, B](f: SqlAccess[A => B], a: SqlAccess[A]) = {
      sqlAccess(r => a.access(r) <*> f.access(r))
    }
  }

  implicit val SqlAccessBind: Bind[SqlAccess] = new Bind[SqlAccess] {
    def bind[A, B](a: SqlAccess[A], f: A => SqlAccess[B]) =
      sqlAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }
}