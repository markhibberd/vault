package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowValue[L, A] {
  def fold[X](value: SqlValue[L, A] => X, nul: => X): X

  def foldV[X](sqlErr: SqlException => X, sqlValue: A => X, nul: => X): X =
    fold(x => x.fold(sqlErr, sqlValue), nul)

  def isNull: Boolean = foldV(_ => false, _ => false, true)
  def isError: Boolean = foldV(_ => true, _ => false, false)
  def isValue: Boolean = foldV(_ => false, _ => true, false)

  def getError: Option[SqlException] =
    fold(_.getError, None)

  def getErrorOr(e: => SqlException): SqlException =
    getError getOrElse e

  def getValue: Option[A] =
    fold(_.getValue, None)

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getSqlValue: Option[SqlValue[L, A]] =
    fold(Some(_), None)

  def getSqlValueOr(v: => SqlValue[L, A]): SqlValue[L, A] =
    getSqlValue getOrElse v

  def printStackTraceOr(value: A => Unit, nul: => Unit): Unit =
    fold(_.printStackTraceOr(value), nul)

  def map[B](f: A => B): RowValue[L, B] =
    fold (x => (x map f).toRowAccess, rowNull)

  def flatMap[B](f: A => RowValue[L, B]): RowValue[L, B] =
    foldV (rowError(_), f, rowNull)

  def possiblyNull: RowValue[L, Option[A]] =
    // fold(v => v.toRowAccess ∘ (_.η[Option]), none[A].η[RowValue])
    error("")

  def possiblyNullOr(d: => A): RowValue[L, A] =
    possiblyNull map (_ getOrElse d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

trait RowValues {
  def rowError[L, A](e: SqlException): RowValue[L, A] = /*new RowValue[A] {
    def fold[X](v: SqlValue[A] => X, nul: => X) =
      v(sqlError(e))
  } */
    error("todo")

  def rowValue[L, A](a: A): RowValue[L, A] = /* new RowValue[A] {
    def fold[X](v: SqlValue[A] => X, nul: => X) =
      v(a.η[SqlValue])
  } */
    error("todo")

  def rowNull[L, A]: RowValue[L, A] = /* new RowValue[A] {
    def fold[X](v: SqlValue[A] => X, nul: => X) = nul
  } */
    error("todo")

  def tryRowValue[L, A](a: => A): RowValue[L, A] =
    // trySqlValue(a).toRowAccess
    error("todo")

//  implicit val RowValueInjective = Injective[RowValue]
//
//  implicit val RowValueFunctor: Functor[RowValue] = new Functor[RowValue] {
//    def fmap[A, B](r: RowValue[A], f: A => B) =
//      r fold (x => (x ∘ f).toRowAccess, rowNull)
//  }
//
//  implicit val RowValuePure: Pure[RowValue] = new Pure[RowValue] {
//    def pure[A](a: => A) =
//      a.η[SqlValue].toRowAccess
//  }
//
//  implicit val RowValueApply: Apply[RowValue] = new Apply[RowValue] {
//    def apply[A, B](f: RowValue[A => B], a: RowValue[A]) =
//      f foldV (rowError(_), ff => a foldV (rowError(_), aa => rowValue(ff(aa)), rowNull), rowNull)
//  }
//
//  implicit val RowValueBind: Bind[RowValue] = new Bind[RowValue] {
//    def bind[A, B](a: RowValue[A], f: A => RowValue[B]) =
//      a foldV (rowError(_), f, rowNull)
//  }
//
//  implicit val RowValueEach: Each[RowValue] = new Each[RowValue] {
//    def each[A](e: RowValue[A], f: A => Unit) =
//      e foldV (_ => (), f, ())
//  }
//
//  implicit val RowAccessIndex: Index[RowValue] = new Index[RowValue] {
//    def index[A](a: RowValue[A], i: Int) = a.getValue filter (_ => i == 0)
//  }
//
//  implicit val RowAccessLength: Length[RowValue] = new Length[RowValue] {
//    def len[A](a: RowValue[A]) =
//      a foldV (_ => 0, _ => 1, 0)
//  }
//
//  implicit val RowAccessFoldable: Foldable[RowValue] = new Foldable[RowValue] {
//    override def foldLeft[A, B](e: RowValue[A], b: B, f: (B, A) => B) =
//      e foldV (_ => b, f(b, _), b)
//
//    override def foldRight[A, B](e: RowValue[A], b: => B, f: (A, => B) => B) =
//      e foldV (_ => b, f(_, b), b)
//  }
//
//  implicit val RowAccessTraverse: Traverse[RowValue] = new Traverse[RowValue] {
//    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowValue[A]): F[RowValue[B]] =
//      as foldV ((e: SqlException) => rowError(e).η[F], v => f(v) ∘ (rowValue(_)), rowNull.η[F])
//  }
//
//  implicit def RowAccessShow[A: Show]: Show[RowValue[A]] = new Show[RowValue[A]] {
//    def show(a: RowValue[A]) =
//      a foldV (
//              e => ("row-error(" + e + ")").toList
//            , a => ("row-value(" + a.shows + ")").toList
//            , "row-null".toList
//            )
//  }
//
//  implicit def RowAccessEqual[A: Equal]: Equal[RowValue[A]] = new Equal[RowValue[A]] {
//    def equal(a1: RowValue[A], a2: RowValue[A]) =
//      a1 fold (t => a2 fold (u => t === u, false), a2.isNull)
//  }
//
//  implicit def RowAccessZero[A: Zero]: Zero[RowValue[A]] = zero(rowValue(∅[A]))
}
