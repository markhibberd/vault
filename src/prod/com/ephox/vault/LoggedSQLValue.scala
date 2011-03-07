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

  def toEither =
    fold(Left(_), Right(_))

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

  implicit def LoggedSQLValueEach[L]: Each[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Each[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def each[A](a: LoggedSQLValue[L, A], f: A => Unit) =
      a.fold(_ => (), f)
  }

  implicit def LoggedSQLValueIndex[L]: Index[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Index[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def index[A](a: LoggedSQLValue[L, A], n: Int) =
      if(n== 0) a.fold(_ => None, (Some(_))) else None
  }

  implicit def LoggedSQLValueFoldable[L]: Foldable[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Foldable[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    override def foldRight[A, B](t: LoggedSQLValue[L, A], b: => B, f: (A, => B) => B) =
      t.fold(_ => b, a => f(a, b))
  }

  implicit def LoggedSQLValueTraverse[L]: Traverse[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Traverse[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], t: LoggedSQLValue[L, A]) =
      t.fold(e => loggedSqlException(e).η[F], a => f(a) ∘ (loggedSqlValue[L](_)))
  }

  implicit def LoggedSQLValuePlus[L]: Plus[({type λ[α]=LoggedSQLValue[L, α]})#λ] = new Plus[({type λ[α]=LoggedSQLValue[L, α]})#λ] {
    def plus[A](a1: LoggedSQLValue[L, A], a2: => LoggedSQLValue[L, A]) =
      a1.fold(_ => a2, _ => a1)
  }

  implicit def LoggedSQLValueShow[L, A : Show]: Show[LoggedSQLValue[L, A]] = new Show[LoggedSQLValue[L, A]] {
    def show(a: LoggedSQLValue[L, A]) =
      a fold(
              e => ("sql-error(" + e + ")").toList
            , a => ("sql-value(" + a.shows + ")").toList
            )

  }

  implicit def LoggedSQLValueEqual[L, A: Equal]: Equal[LoggedSQLValue[L, A]] = {
    implicit val EqualSQLException: Equal[SQLException] = equalA
    Equal.EitherEqual[SQLException, A] ∙ (_.toEither)
  }

  implicit def LoggedSQLValueOrder[L, A: Order]: Order[LoggedSQLValue[L, A]] = {
    implicit val OrderSQLException: Order[SQLException] = new Order[SQLException] {
      def order(a1: SQLException, a2: SQLException) = EQ
    }
    Order.EitherOrder[SQLException, A] ∙ (_.toEither)
  }

  implicit def LoggedSQLValueZero[L, A: Zero]: Zero[LoggedSQLValue[L, A]] = zero(loggedSqlValue[L](∅[A]))
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