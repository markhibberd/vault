package com.ephox.vault

import scalaz._
import Scalaz._

import LoggedSQLValue._
import java.sql.SQLException

sealed trait LoggedSQLValue[A] {
  val log: LOG
  def fold[X](ex: SQLException => X, value: A => X): X

  def isError =
    fold(_ => true, _ => false)

  def isValue =
    !isError

  def getError =
    fold(Some(_), _ => None)

  def getErrorOr(e: => SQLException) =
    getError getOrElse e

  def getValue =
    fold(_ => None, Some(_))

  def getValueOr(v: => A) =
    getValue getOrElse v

  def toValidation =
    fold(success(_), failure(_))

  def toRowAccess: RowAccess[A] =
    fold(rowAccessError, rowAccessValue(_))

  def printStackTraceOr(f: A => Unit) =
    fold(_.printStackTrace, f)

  def map[B](f: A => B): LoggedSQLValue[B] = new LoggedSQLValue[B] {
    val log = LoggedSQLValue.this.log
    def fold[X](err: SQLException => X, value: B => X) =
      LoggedSQLValue.this.fold(err, value compose f)
  }

  def flatMap[B](f: A => LoggedSQLValue[B]): LoggedSQLValue[B] =
    fold(loggedSqlException(_) setLog log, a => {
      val v = f(a)
      v.fold[LoggedSQLValue[B]](loggedSqlException(_), loggedSqlValue(_)) :+-> (log |+| v.log)
    })

  def withLog(f: LOG => LOG): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val log = f(LoggedSQLValue.this.log)
    def fold[X](ex: SQLException => X, value: A => X) =
      LoggedSQLValue.this.fold(ex, value)
  }

  def setLog(l: LOG) = withLog(_ => l)

  def :+->(l: LOG): LoggedSQLValue[A] = withLog(l |+| _)

  def <-+:(l: LOG) = withLog(_ |+| l)

  def resetLog = withLog(_ => ∅[LOG])

  // CAUTION side-effect
  def flush(f: LOG => Unit) = {
    f(log)
    resetLog
  }
}

object LoggedSQLValue {
  type LOG = List[String] // todo use better data structure

  // Pure, Functor, Apply, Bind, Each, Index, Length, Foldable, Traverse, Plus, Empty, Show, Equal, Order, Zero
}

trait LoggedSQLValues {
  def loggedSqlValue[A](v: A): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val log = ∅[LOG]
    def fold[X](ex: SQLException => X, value: A => X) = value(v)
  }

  def loggedSqlException[A](e: SQLException): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val log = ∅[LOG]
    def fold[X](ex: SQLException => X, value: A => X) = ex(e)
  }
}