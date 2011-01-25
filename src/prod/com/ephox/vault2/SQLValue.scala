package com.ephox.vault2

import java.sql.SQLException
import scalaz._
import Scalaz._

sealed trait SQLValue[A] {
  def fold[X](err: SQLException => X, value: A => X): X

  def isSQLException =
    fold(_ => true, _ => false)

  def isValue =
    !isSQLException

  def getSQLException =
    fold(Some(_), _ => None)

  def getSQLExceptionOr(e: => SQLException) =
    getSQLException getOrElse e

  def getValue =
    fold(_ => None, Some(_))

  def getValueOr(v: => A) =
    getValue getOrElse v

  def toEither =
    fold(Left(_), Right(_))

  def toValidation =
    fold(success(_), failure(_))
}

object SQLValue {
  def err[A](e: SQLException): SQLValue[A] = new SQLValue[A] {
    def fold[X](err: SQLException => X, value: A => X) = err(e)
  }

  def value[A](v: A): SQLValue[A] = new SQLValue[A] {
    def fold[X](err: SQLException => X, va: A => X) = va(v)
  }

  implicit def SQLValueInjective = Injective[SQLValue]

  implicit val SQLValueFunctor: Functor[SQLValue] = new Functor[SQLValue] {
    def fmap[A, B](r: SQLValue[A], f: A => B) =
      r fold (err(_), a => value(f(a)))
  }

  implicit val SQLValuePure: Pure[SQLValue] = new Pure[SQLValue] {
    def pure[A](a: => A) =
      value(a)
  }

  implicit val SQLValueApply: Apply[SQLValue] = new Apply[SQLValue] {
    def apply[A, B](f: SQLValue[A => B], a: SQLValue[A]) =
      f fold (err(_), ff => a fold (err(_), aa => value(ff(aa))))
  }

  implicit val SQLValueApplicative: Applicative[SQLValue] = Applicative.applicative

  implicit val SQLValueBind: Bind[SQLValue] = new Bind[SQLValue] {
    def bind[A, B](a: SQLValue[A], f: A => SQLValue[B]) =
      a fold (err(_), f)
  }

  implicit val SQLValueMonad: Monad[SQLValue] = Monad.monad


  implicit val SQLValueEach: Each[SQLValue] = new Each[SQLValue] {
    def each[A](e: SQLValue[A], f: A => Unit) =
      e fold (x => (), f)
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
      as fold ((e: SQLException) => err(e).η[F], v => f(v) ∘ (value(_)))
  }

  implicit val SQLValuePlus: Plus[SQLValue] = new Plus[SQLValue] {
    def plus[A](a1: SQLValue[A], a2: => SQLValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit val SQLValueEmpty: Empty[SQLValue] = new Empty[SQLValue] {
    def empty[A] = err(new SQLException)
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

  implicit def SQLValueZero[A: Zero]: Zero[SQLValue[A]] = zero(value(∅[A]))
}
