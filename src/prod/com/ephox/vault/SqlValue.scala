package com.ephox.vault

import java.sql.SQLException
import scalaz._
import Scalaz._

sealed trait SqlValue[A] {
  def fold[X](err: SQLException => X, value: A => X): X

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

  def toValidation: Validation[SQLException, A] =
    fold(failure(_), success(_))

  def toRowAccess: RowValue[A] =
    fold(rowError, rowValue(_))

  def printStackTraceOr(f: A => Unit) =
    fold(_.printStackTrace, f)

  def map[B](f: A => B): SqlValue[B] = new SqlValue[B] {
    def fold[X](err: SQLException => X, value: B => X) =
      SqlValue.this.fold(err, value compose f)
  }

  def flatMap[B](f: A => SqlValue[B]): SqlValue[B] = new SqlValue[B] {
    def fold[X](err: SQLException => X, value: B => X) =
      SqlValue.this.fold(err, a => f(a) fold (err, value))
  }
}

trait SqlValues {
  def sqlError[A](e: SQLException): SqlValue[A] = new SqlValue[A] {
    def fold[X](err: SQLException => X, value: A => X) = err(e)
  }

  def sqlValue[A](v: A): SqlValue[A] = new SqlValue[A] {
    def fold[X](err: SQLException => X, va: A => X) = va(v)
  }

  def trySqlValue[A](a: => A): SqlValue[A] =
    try {
      a.η[SqlValue]
    } catch {
      case e: SQLException => sqlError(e)
      case e               => throw e
    }

  implicit val SqlValueInjective = Injective[SqlValue]

  implicit val SqlValueFunctor: Functor[SqlValue] = new Functor[SqlValue] {
    def fmap[A, B](r: SqlValue[A], f: A => B) =
      r fold (sqlError(_), a => sqlValue(f(a)))
  }

  implicit val SqlValuePure: Pure[SqlValue] = new Pure[SqlValue] {
    def pure[A](a: => A) =
      sqlValue(a)
  }

  implicit val SqlValueApply: Apply[SqlValue] = new Apply[SqlValue] {
    def apply[A, B](f: SqlValue[A => B], a: SqlValue[A]) =
      f fold (sqlError(_), ff => a fold (sqlError(_), aa => sqlValue(ff(aa))))
  }

  implicit val SqlValueBind: Bind[SqlValue] = new Bind[SqlValue] {
    def bind[A, B](a: SqlValue[A], f: A => SqlValue[B]) =
      a fold (sqlError(_), f)
  }

  implicit val SqlValueEach: Each[SqlValue] = new Each[SqlValue] {
    def each[A](e: SqlValue[A], f: A => Unit) =
      e fold (_ => (), f)
  }

  implicit val SqlValueIndex: Index[SqlValue] = new Index[SqlValue] {
    def index[A](a: SqlValue[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val SqlValueLength: Length[SqlValue] = new Length[SqlValue] {
    def len[A](a: SqlValue[A]) =
      a fold(_ => 0, _ => 1)
  }

  implicit val SqlValueFoldable: Foldable[SqlValue] = new Foldable[SqlValue] {
    override def foldLeft[A, B](e: SqlValue[A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _))

    override def foldRight[A, B](e: SqlValue[A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b))
  }

  implicit val SqlValueTraverse: Traverse[SqlValue] = new Traverse[SqlValue] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: SqlValue[A]): F[SqlValue[B]] =
      as fold ((e: SQLException) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
  }

  implicit val SqlValuePlus: Plus[SqlValue] = new Plus[SqlValue] {
    def plus[A](a1: SqlValue[A], a2: => SqlValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit val SqlValueEmpty: Empty[SqlValue] = new Empty[SqlValue] {
    def empty[A] = sqlError(new SQLException)
  }

  implicit def SqlValueShow[A: Show]: Show[SqlValue[A]] = new Show[SqlValue[A]] {
    def show(a: SqlValue[A]) =
      a fold(
              e => ("error(" + e + ")").toList
            , a => ("value(" + a.shows + ")").toList
            )
  }

  implicit def SqlValueEqual[A: Equal]: Equal[SqlValue[A]] = {
    implicit val EqualSQLException: Equal[SQLException] = equalA
    Equal.EitherEqual[SQLException, A] ∙ (_.toEither)
  }

  implicit def SqlValueOrder[A: Order]: Order[SqlValue[A]] = {
    implicit val OrderSQLException: Order[SQLException] = new Order[SQLException] {
      def order(a1: SQLException, a2: SQLException) = EQ
    }
    Order.EitherOrder[SQLException, A] ∙ (_.toEither)
  }

  implicit def SqlValueZero[A: Zero]: Zero[SqlValue[A]] = zero(sqlValue(∅[A]))
}
