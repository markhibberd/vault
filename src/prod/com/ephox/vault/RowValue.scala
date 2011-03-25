package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowValue[L, A] extends NewType[Logger[L, Option[Either[SqlException, A]]]] {
  def fold[X](sqlErr: SqlException => X, sqlValue: A => X, nul: => X): X =
    value.over match {
      case None           => nul
      case Some(Left(e))  => sqlErr(e)
      case Some(Right(a)) => sqlValue(a)
    }

  def isNull: Boolean = fold(_ => false, _ => false, true)
  def isError: Boolean = fold(_ => true, _ => false, false)
  def isValue: Boolean = fold(_ => false, _ => true, false)

  def getError: Option[SqlException] =
    fold(Some(_), _ => None, None)

  def getErrorOr(e: => SqlException): SqlException =
    getError getOrElse e

  def getValue: Option[A] =
    fold(_ => None, Some(_), None)

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getSqlValue: Option[SqlValue[L, A]] =
    fold(e => Some(sqlError(e)), a => Some(sqlValue(a)), None)

  def getSqlValueSeq: SqlValue[L, Option[A]] =
    getSqlValue.sequence[({type λ[α]= SqlValue[L, α]})#λ, A]

  def getSqlValueOr(v: => SqlValue[L, A]): SqlValue[L, A] =
    getSqlValue getOrElse v

  def printStackTraceOr(v: A => Unit, nul: => Unit): Unit =
    fold(_ => (), v, nul)

  def map[B](f: A => B): RowValue[L, B] =
    fold(rowError(_), a => rowValue(f(a)), rowNull)

  def flatMap[B](f: A => RowValue[L, B]): RowValue[L, B] =
    fold(rowError(_), f, rowNull)

  def isNotNullWithMessage(s: String): SqlValue[L, A] =
    fold(sqlError(_), sqlValue(_), sqlError(new SqlException(s)))

  def isNotNull: SqlValue[L, A] =
    isNotNullWithMessage("is not null")

  def possiblyNull: RowValue[L, Option[A]] =
    fold(rowError(_), a => rowValue(Some(a)), rowValue(None))

  def possiblyNullOr(d: => A): RowValue[L, A] =
    possiblyNull map (_ getOrElse d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

trait RowValues {
  def rowError[L, A](e: SqlException): RowValue[L, A] = new RowValue[L, A] {
    val value = (Some(Left(e)): Option[Either[SqlException, A]]).logger[L]
  }

  def rowValue[L, A](a: A): RowValue[L, A] = new RowValue[L, A] {
    val value = (Some(Right(a)): Option[Either[SqlException, A]]).logger[L]
  }

  def rowNull[L, A]: RowValue[L, A] = new RowValue[L, A] {
    val value = (None: Option[Either[SqlException, A]]).logger[L]
  }

  def tryRowValue[L, A](a: => A): RowValue[L, A] =
    trySqlValue(a).toRowValue

  implicit def RowValueInjective[L] = Injective[({type λ[α]= RowValue[L, α]})#λ]

  implicit def RowValueFunctor[L]: Functor[({type λ[α]= RowValue[L, α]})#λ] = new Functor[({type λ[α]= RowValue[L, α]})#λ] {
    def fmap[A, B](r: RowValue[L, A], f: A => B) =
      r map f
  }

  implicit def RowValuePure[L]: Pure[({type λ[α]= RowValue[L, α]})#λ] = new Pure[({type λ[α]= RowValue[L, α]})#λ] {
    def pure[A](a: => A) =
      rowValue(a)
  }

  implicit def RowValueApply[L]: Apply[({type λ[α]= RowValue[L, α]})#λ] = new Apply[({type λ[α]= RowValue[L, α]})#λ] {
    def apply[A, B](f: RowValue[L, A => B], a: RowValue[L, A]) =
      f fold (rowError(_), ff => a fold (rowError(_), aa => rowValue(ff(aa)), rowNull), rowNull)
  }

  implicit def RowValueApplicative[L]: Applicative[({type λ[α]= RowValue[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowValue[L, α]})#λ]

  implicit def RowValueBind[L]: Bind[({type λ[α]= RowValue[L, α]})#λ] = new Bind[({type λ[α]= RowValue[L, α]})#λ] {
    def bind[A, B](a: RowValue[L, A], f: A => RowValue[L, B]) =
      a fold (rowError(_), f, rowNull)
  }

  implicit def RowValueMonad[L]: Monad[({type λ[α]= RowValue[L, α]})#λ] = Monad.monad[({type λ[α]= RowValue[L, α]})#λ]

  implicit def RowValueEach[L]: Each[({type λ[α]= RowValue[L, α]})#λ] = new Each[({type λ[α]= RowValue[L, α]})#λ] {
    def each[A](e: RowValue[L, A], f: A => Unit) =
      e fold (_ => (), f, ())
  }

  implicit def RowAccessIndex[L]: Index[({type λ[α]= RowValue[L, α]})#λ] = new Index[({type λ[α]= RowValue[L, α]})#λ] {
    def index[A](a: RowValue[L, A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit def RowAccessLength[L]: Length[({type λ[α]= RowValue[L, α]})#λ] = new Length[({type λ[α]= RowValue[L, α]})#λ] {
    def len[A](a: RowValue[L, A]) =
      a fold (_ => 0, _ => 1, 0)
  }

  implicit def RowAccessFoldable[L]: Foldable[({type λ[α]= RowValue[L, α]})#λ] = new Foldable[({type λ[α]= RowValue[L, α]})#λ] {
    override def foldLeft[A, B](e: RowValue[L, A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _), b)

    override def foldRight[A, B](e: RowValue[L, A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b), b)
  }

  implicit def RowAccessTraverse[L]: Traverse[({type λ[α]= RowValue[L, α]})#λ] = new Traverse[({type λ[α]= RowValue[L, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowValue[L, A]): F[RowValue[L, B]] =
      as fold ((e: SqlException) => rowError(e).η[F], v => f(v) ∘ (rowValue(_)), rowNull.η[F])
  }

  implicit def RowAccessShow[L, A: Show]: Show[RowValue[L, A]] = new Show[RowValue[L, A]] {
    def show(a: RowValue[L, A]) =
      a fold (
              e => ("row-error(" + e + ")").toList
            , a => ("row-value(" + a.shows + ")").toList
            , "row-null".toList
            )
  }

  implicit def RowAccessEqual[L, A: Equal]: Equal[RowValue[L, A]] = new Equal[RowValue[L, A]] {
    def equal(a1: RowValue[L, A], a2: RowValue[L, A]) =
      a1 fold (
        _ => a2.isError
      , t => a2 fold (_ => false, t === _, false)
      , a2.isNull)
  }

  implicit def RowAccessZero[L, A: Zero]: Zero[RowValue[L, A]] = zero(rowValue(∅[A]))
}
