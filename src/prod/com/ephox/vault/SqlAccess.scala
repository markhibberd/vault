package com.ephox.vault

import scalaz._, Scalaz._

sealed trait SqlAccess[A] {
  val access: Row => SqlValue[A]

  import SqlValue._
  import RowAccess._

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

  def map[B](f: A => B): SqlAccess[B] = new SqlAccess[B] {
    val access = (r: Row) => SqlAccess.this.access(r) map f
  }

  def flatMap[B](f: A => SqlAccess[B]): SqlAccess[B] = new SqlAccess[B] {
    val access = (r: Row) => SqlAccess.this.access(r) flatMap (a => f(a).access(r))
  }

  def mapSqlValue[B](f: SqlValue[A] => SqlValue[B]): SqlAccess[B] = new SqlAccess[B] {
    val access = (r: Row) => f(SqlAccess.this.access(r))
  }

  def toRowAccess: RowAccess[A] =
    rowAccess(r => access(r).toRowValue)

  /**
   * Return the log associated with this value.
   */
  def log: Row => LOG = access(_).log

  /**
   * Sets the log to the given value.
   */
  def setLog(k: LOG): SqlAccess[A] =
    mapSqlValue(_ setLog k)

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG => LOG): SqlAccess[A] =
    mapSqlValue(_ withLog k)

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: LOGV => LOGV): SqlAccess[A] =
    mapSqlValue(_ withEachLog k)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LOGV): SqlAccess[A] =
    mapSqlValue(_ :+-> e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :->>(e: Either[SqlException, A] => LOGV): SqlAccess[A] =
    mapSqlValue(_ :->> e)

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): SqlAccess[A] =
    mapSqlValue(e <-+: _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-:(e: Either[SqlException, A] => LOGV): SqlAccess[A] =
    mapSqlValue(e <<-: _)

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): SqlAccess[A] =
    mapSqlValue(_ :++-> e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :+->>(e: Either[SqlException, A] => LOG): SqlAccess[A] =
    mapSqlValue(_ :+->> e)

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): SqlAccess[A] =
    mapSqlValue(e <-++: _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-+:(e: Either[SqlException, A] => LOG): SqlAccess[A] =
    mapSqlValue(e <<-+: _)

  /**
   * Set the log to be empty.
   */
  def resetLog: SqlAccess[A] =
    mapSqlValue(_.resetLog)
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

  implicit val SqlAccessBind: Bind[SqlAccess] = new Bind[SqlAccess] {
    def bind[A, B](a: SqlAccess[A], f: A => SqlAccess[B]) =
      sqlAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }
}