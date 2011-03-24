package com.ephox.vault

import java.sql.SQLException
import scalaz._
import Scalaz._

sealed trait RowValue[A] {
  def fold[X](value: SQLValue[A] => X, nul: => X): X

  def foldV[X](sqlErr: SQLException => X, sqlValue: A => X, nul: => X): X =
    fold(x => x.fold(sqlErr, sqlValue), nul)

  def isNull: Boolean = foldV(_ => false, _ => false, true)
  def isError: Boolean = foldV(_ => true, _ => false, false)
  def isValue: Boolean = foldV(_ => false, _ => true, false)

  def getError =
    fold(Some(_), None)

  def getErrorOr(e: => SQLException) =
    getError getOrElse e

  def getValue =
    fold(_.getValue, None)

  def getValueOr(v: => A) =
    getValue getOrElse v

  def getSQLValue =
    fold(Some(_), None)

  def getSQLValueOr(v: => SQLValue[A]) =
    getSQLValue getOrElse v

  def printStackTraceOr(value: A => Unit, nul: => Unit) =
    fold(_.printStackTraceOr(value), nul)

  def map[B](f: A => B): RowValue[B] =
    fold (x => (x ∘ f).toRowAccess, rowNull)

  def flatMap[B](f: A => RowValue[B]): RowValue[B] =
    foldV (rowError(_), f, rowNull)

  def possiblyNull: RowValue[Option[A]] =
    fold(v => v.toRowAccess ∘ (_.η[Option]), none[A].η[RowValue])

  def possiblyNullOr(d: => A): RowValue[A] =
    possiblyNull ∘ (_ getOrElse d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

trait RowValues {
  def rowError[A](e: SQLException): RowValue[A] = new RowValue[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) =
      v(sqlError(e))
  }

  def rowValue[A](a: A): RowValue[A] = new RowValue[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) =
      v(a.η[SQLValue])
  }

  def rowNull[A]: RowValue[A] = new RowValue[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) = nul
  }

  def tryRowValue[A](a: => A): RowValue[A] =
    trySQLValue(a).toRowAccess

  implicit val RowValueInjective = Injective[RowValue]

  implicit val RowValueFunctor: Functor[RowValue] = new Functor[RowValue] {
    def fmap[A, B](r: RowValue[A], f: A => B) =
      r fold (x => (x ∘ f).toRowAccess, rowNull)
  }

  implicit val RowValuePure: Pure[RowValue] = new Pure[RowValue] {
    def pure[A](a: => A) =
      a.η[SQLValue].toRowAccess
  }

  implicit val RowValueApply: Apply[RowValue] = new Apply[RowValue] {
    def apply[A, B](f: RowValue[A => B], a: RowValue[A]) =
      f foldV (rowError(_), ff => a foldV (rowError(_), aa => rowValue(ff(aa)), rowNull), rowNull)
  }

  implicit val RowValueBind: Bind[RowValue] = new Bind[RowValue] {
    def bind[A, B](a: RowValue[A], f: A => RowValue[B]) =
      a foldV (rowError(_), f, rowNull)
  }

  implicit val RowValueEach: Each[RowValue] = new Each[RowValue] {
    def each[A](e: RowValue[A], f: A => Unit) =
      e foldV (_ => (), f, ())
  }

  implicit val RowAccessIndex: Index[RowValue] = new Index[RowValue] {
    def index[A](a: RowValue[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val RowAccessLength: Length[RowValue] = new Length[RowValue] {
    def len[A](a: RowValue[A]) =
      a foldV (_ => 0, _ => 1, 0)
  }

  implicit val RowAccessFoldable: Foldable[RowValue] = new Foldable[RowValue] {
    override def foldLeft[A, B](e: RowValue[A], b: B, f: (B, A) => B) =
      e foldV (_ => b, f(b, _), b)

    override def foldRight[A, B](e: RowValue[A], b: => B, f: (A, => B) => B) =
      e foldV (_ => b, f(_, b), b)
  }

  implicit val RowAccessTraverse: Traverse[RowValue] = new Traverse[RowValue] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowValue[A]): F[RowValue[B]] =
      as foldV ((e: SQLException) => rowError(e).η[F], v => f(v) ∘ (rowValue(_)), rowNull.η[F])
  }

  implicit def RowAccessShow[A: Show]: Show[RowValue[A]] = new Show[RowValue[A]] {
    def show(a: RowValue[A]) =
      a foldV (
              e => ("row-error(" + e + ")").toList
            , a => ("row-value(" + a.shows + ")").toList
            , "row-null".toList
            )
  }

  implicit def RowAccessEqual[A: Equal]: Equal[RowValue[A]] = new Equal[RowValue[A]] {
    def equal(a1: RowValue[A], a2: RowValue[A]) =
      a1 fold (t => a2 fold (u => t === u, false), a2.isNull)
  }

  implicit def RowAccessZero[A: Zero]: Zero[RowValue[A]] = zero(rowValue(∅[A]))
}
