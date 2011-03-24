package com.ephox.vault

import java.sql.SQLException
import scalaz._
import Scalaz._

sealed trait SQLValue[A] {
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

  def map[B](f: A => B): SQLValue[B] = new SQLValue[B] {
    def fold[X](err: SQLException => X, value: B => X) =
      SQLValue.this.fold(err, value compose f)
  }

  def flatMap[B](f: A => SQLValue[B]): SQLValue[B] = new SQLValue[B] {
    def fold[X](err: SQLException => X, value: B => X) =
      SQLValue.this.fold(err, a => f(a) fold (err, value))
  }
}

trait SQLValues {
  def sqlError[A](e: SQLException): SQLValue[A] = new SQLValue[A] {
    def fold[X](err: SQLException => X, value: A => X) = err(e)
  }

  def sqlValue[A](v: A): SQLValue[A] = new SQLValue[A] {
    def fold[X](err: SQLException => X, va: A => X) = va(v)
  }

  def trySQLValue[A](a: => A): SQLValue[A] =
    try {
      a.η[SQLValue]
    } catch {
      case e: SQLException => sqlError(e)
      case e               => throw e
    }

  implicit val SQLValueInjective = Injective[SQLValue]

  implicit val SQLValueFunctor: Functor[SQLValue] = new Functor[SQLValue] {
    def fmap[A, B](r: SQLValue[A], f: A => B) =
      r fold (sqlError(_), a => sqlValue(f(a)))
  }

  implicit val SQLValuePure: Pure[SQLValue] = new Pure[SQLValue] {
    def pure[A](a: => A) =
      sqlValue(a)
  }

  implicit val SQLValueApply: Apply[SQLValue] = new Apply[SQLValue] {
    def apply[A, B](f: SQLValue[A => B], a: SQLValue[A]) =
      f fold (sqlError(_), ff => a fold (sqlError(_), aa => sqlValue(ff(aa))))
  }

  implicit val SQLValueBind: Bind[SQLValue] = new Bind[SQLValue] {
    def bind[A, B](a: SQLValue[A], f: A => SQLValue[B]) =
      a fold (sqlError(_), f)
  }

  implicit val SQLValueEach: Each[SQLValue] = new Each[SQLValue] {
    def each[A](e: SQLValue[A], f: A => Unit) =
      e fold (_ => (), f)
  }

  implicit val SQLValueIndex: Index[SQLValue] = new Index[SQLValue] {
    def index[A](a: SQLValue[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val SQLValueLength: Length[SQLValue] = new Length[SQLValue] {
    def len[A](a: SQLValue[A]) =
      a fold(_ => 0, _ => 1)
  }

  implicit val SQLValueFoldable: Foldable[SQLValue] = new Foldable[SQLValue] {
    override def foldLeft[A, B](e: SQLValue[A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _))

    override def foldRight[A, B](e: SQLValue[A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b))
  }

  implicit val SQLValueTraverse: Traverse[SQLValue] = new Traverse[SQLValue] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: SQLValue[A]): F[SQLValue[B]] =
      as fold ((e: SQLException) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
  }

  implicit val SQLValuePlus: Plus[SQLValue] = new Plus[SQLValue] {
    def plus[A](a1: SQLValue[A], a2: => SQLValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit val SQLValueEmpty: Empty[SQLValue] = new Empty[SQLValue] {
    def empty[A] = sqlError(new SQLException)
  }

  implicit def SQLValueShow[A: Show]: Show[SQLValue[A]] = new Show[SQLValue[A]] {
    def show(a: SQLValue[A]) =
      a fold(
              e => ("error(" + e + ")").toList
            , a => ("value(" + a.shows + ")").toList
            )
  }

  implicit def SQLValueEqual[A: Equal]: Equal[SQLValue[A]] = {
    implicit val EqualSQLException: Equal[SQLException] = equalA
    Equal.EitherEqual[SQLException, A] ∙ (_.toEither)
  }

  implicit def SQLValueOrder[A: Order]: Order[SQLValue[A]] = {
    implicit val OrderSQLException: Order[SQLException] = new Order[SQLException] {
      def order(a1: SQLException, a2: SQLException) = EQ
    }
    Order.EitherOrder[SQLException, A] ∙ (_.toEither)
  }

  implicit def SQLValueZero[A: Zero]: Zero[SQLValue[A]] = zero(sqlValue(∅[A]))
}
