package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import RowAccess._

sealed trait SqlAccess[A] {
  private def value = this match {
    case SqlAccess_(v) => v
  }

  import SqlAccess._

  def access: Row => SqlValue[A] =
    r => value.over(r) :++-> value.written

  def toRowAccess: RowAccess[A] =
    rowAccess(r => access(r).toRowValue)

  /**
   * lifted to row-access.
   */
  def -|>[T](iter: IterV[A, T]): RowQueryConnect[IterV[A, T]] =
   toRowAccess -|> iter

  /**
   * lifted to row-access.
   */
  def -||>[T](iter: IterV[A, T]): RowQueryConnect[T] =
    toRowAccess -||> iter

  def map[B](f: A => B): SqlAccess[B] =
    sqlAccess((r: Row) => SqlAccess.this.access(r) map f)

  def flatMap[B](f: A => SqlAccess[B]): SqlAccess[B] = sqlAccess((r: Row) => SqlAccess.this.access(r) flatMap (a => f(a).access(r)))

  def mapSqlValue[B](f: SqlValue[A] => SqlValue[B]): SqlAccess[B] = sqlAccess((r: Row) => f(SqlAccess.this.access(r)))
}
private final case class SqlAccess_[A](v: WLOG[Row => SqlValue[A]]) extends SqlAccess[A]

object SqlAccess extends SqlAccesss

trait SqlAccesss {
  def sqlAccess[A](f: Row => SqlValue[A]): SqlAccess[A] = SqlAccess_(f set ∅[LOG])

  implicit val SqlAccessFunctor: Functor[SqlAccess] = new Functor[SqlAccess] {
    def fmap[A, B](k: SqlAccess[A], f: A => B) =
      k map f
  }

  implicit val SqlAccessPure: Pure[SqlAccess] = new Pure[SqlAccess] {
    def pure[A](a: => A) =
      sqlAccess(_ => a.η[SqlValue])
  }

  implicit val SqlAccessBind: Bind[SqlAccess] = new Bind[SqlAccess] {
    def bind[A, B](a: SqlAccess[A], f: A => SqlAccess[B]) =
      sqlAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }
}
