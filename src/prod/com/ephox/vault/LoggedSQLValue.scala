package com.ephox.vault

import scalaz._
import Scalaz._

import LoggedSQLValue._
import java.sql.SQLException

sealed trait LoggedSQLValue[L, A] {
  type LOG =
    LOGC[L]

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

  def map[B](f: A => B): LoggedSQLValue[L, B] = new LoggedSQLValue[L, B] {
    val log = LoggedSQLValue.this.log
    def fold[X](err: SQLException => X, value: B => X) =
      LoggedSQLValue.this.fold(err, value compose f)
  }

  def flatMap[B](f: A => LoggedSQLValue[L, B]): LoggedSQLValue[L, B] =
    fold(loggedSqlException(_) setLog log, a => {
      val v = f(a)
      v.fold[LoggedSQLValue[L, B]](loggedSqlException(_), loggedSqlValue(_)) :+-> (log |+| v.log)
    })

  def withLog(f: LOG => LOG): LoggedSQLValue[L, A] = new LoggedSQLValue[L, A] {
    val log = f(LoggedSQLValue.this.log)
    def fold[X](ex: SQLException => X, value: A => X) =
      LoggedSQLValue.this.fold(ex, value)
  }

  def setLog(l: LOG) = withLog(_ => l)

  def :+->(l: LOG) = withLog(l |+| _)

  def <-+:(l: LOG) = withLog(_ |+| l)

  def resetLog = withLog(_ => ∅[LOG])

  // CAUTION side-effect
  def flush(f: LOG => Unit) = {
    f(log)
    resetLog
  }
}

object LoggedSQLValue {
  type LOGC[C] = List[C] // todo use better data structure

  implicit def LoggedSQLValueFunctor[L]: Functor[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Functor[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def fmap[A, B](r: LoggedSQLValue[L, A], f: A => B) =
      r map f
  }

  implicit def LoggedSQLValuePure[L]: Pure[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Pure[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def pure[A](a: => A) =
      loggedSqlValue(a)
  }

  implicit def LoggedSQLValueApply[L]: Apply[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Apply[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def apply[A, B](f: LoggedSQLValue[L, A => B], a: LoggedSQLValue[L, A]) =
      for {
        ff <- f
        aa <- a
      } yield ff(aa)
  }

  implicit def LoggedSQLValueBind[L]: Bind[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Bind[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def bind[A, B](a: LoggedSQLValue[L, A], f: A => LoggedSQLValue[L, B]) =
      a flatMap f
  }

  // Each, Index, Length, Foldable, Traverse, Plus, Empty, Show, Equal, Order, Zero
}

trait LoggedSQLValues {
  def loggedSqlValue[L] = new (Id ~> (({type λ[α]=LoggedSQLValue[L, α]})#λ)) {
    def apply[A](v: A) = new LoggedSQLValue[L, A] {
      val log = ∅[LOG]
      def fold[X](ex: SQLException => X, value: A => X) = value(v)
    }
  }

  def loggedSqlException[L, A](e: SQLException): LoggedSQLValue[L, A] = new LoggedSQLValue[L, A] {
    val log = ∅[LOG]
    def fold[X](ex: SQLException => X, value: A => X) = ex(e)
  }
}