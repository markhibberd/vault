package com.ephox.vault

import scalaz._, iteratee._, Scalaz._
import SqlValue._
import RowAccess._

sealed trait SqlAccess[A] {
  import SqlAccess._

  def access: Row => SqlValue[A] =
    this match {
      case SqlAccess_(v) => v
    }

  def toRowAccess: RowAccess[A] =
    rowAccess(r => access(r).toRowValue)

  /**
   * lifted to row-access.
   */
  def -|>[T](iter: Iteratee[A, T]): RowQueryConnect[Iteratee[A, T]] =
   toRowAccess -|> iter

  /**
   * lifted to row-access.
   */
  def -||>[T](iter: Iteratee[A, T]): RowQueryConnect[T] =
    toRowAccess -||> iter

  def map[B](f: A => B): SqlAccess[B] =
    sqlAccess((r: Row) => SqlAccess.this.access(r) map f)

  def flatMap[B](f: A => SqlAccess[B]): SqlAccess[B] = sqlAccess((r: Row) => SqlAccess.this.access(r) flatMap (a => f(a).access(r)))

  def mapSqlValue[B](f: SqlValue[A] => SqlValue[B]): SqlAccess[B] = sqlAccess((r: Row) => f(SqlAccess.this.access(r)))
}
private final case class SqlAccess_[A](v: Row => SqlValue[A]) extends SqlAccess[A]

object SqlAccess extends SqlAccesss

trait SqlAccesss {
  def sqlAccess[A](f: Row => SqlValue[A]): SqlAccess[A] = SqlAccess_(f)

  implicit val SqlAccessMonad: Monad[SqlAccess] = new Monad[SqlAccess] {
    def bind[A, B](a: SqlAccess[A])(f: A => SqlAccess[B]) =
      sqlAccess(r => a.access(r) flatMap (a => f(a) access (r)))

    def point[A](a: => A) =
      sqlAccess(_ => a.point[SqlValue])
  }
}
