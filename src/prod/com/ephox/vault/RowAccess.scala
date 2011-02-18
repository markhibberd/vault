package com.ephox.vault

import java.sql.SQLException
import scalaz._
import Scalaz._

sealed trait RowAccess[A] {
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

  def printStackTraceOr(value: A => Unit, nul: => Unit) =
    fold(_.printStackTraceOr(value), nul)
}

object RowAccess {
  def err[A](e: SQLException): RowAccess[A] = new RowAccess[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) =
      v(sqlErr(e))
  }

  def value[A](a: A): RowAccess[A] = new RowAccess[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) =
      v(a.η[SQLValue])
  }

  def nul[A]: RowAccess[A] = new RowAccess[A] {
    def fold[X](v: SQLValue[A] => X, nul: => X) = nul
  }

  implicit val RowAccessInjective = Injective[RowAccess]

  implicit val RowAccessFunctor: Functor[RowAccess] = new Functor[RowAccess] {
    def fmap[A, B](r: RowAccess[A], f: A => B) =
      r fold (x => (x ∘ f).toRowAccess, nul)
  }

  implicit val RowAccessPure: Pure[RowAccess] = new Pure[RowAccess] {
    def pure[A](a: => A) =
      a.η[SQLValue].toRowAccess
  }

  implicit val RowAccessApply: Apply[RowAccess] = new Apply[RowAccess] {
    def apply[A, B](f: RowAccess[A => B], a: RowAccess[A]) =
      f foldV (err(_), ff => a foldV (err(_), aa => value(ff(aa)), nul), nul)
  }

  implicit val RowAccessBind: Bind[RowAccess] = new Bind[RowAccess] {
    def bind[A, B](a: RowAccess[A], f: A => RowAccess[B]) =
      a foldV (err(_), f, nul)
  }

  implicit val RowAccessEach: Each[RowAccess] = new Each[RowAccess] {
    def each[A](e: RowAccess[A], f: A => Unit) =
      e foldV (_ => (), f, ())
  }

  implicit val RowAccessIndex: Index[RowAccess] = new Index[RowAccess] {
    def index[A](a: RowAccess[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val RowAccessLength: Length[RowAccess] = new Length[RowAccess] {
    def len[A](a: RowAccess[A]) =
      a foldV (_ => 0, _ => 1, 0)
  }

  implicit val RowAccessFoldable: Foldable[RowAccess] = new Foldable[RowAccess] {
    override def foldLeft[A, B](e: RowAccess[A], b: B, f: (B, A) => B) =
      e foldV (_ => b, f(b, _), b)

    override def foldRight[A, B](e: RowAccess[A], b: => B, f: (A, => B) => B) =
      e foldV (_ => b, f(_, b), b)
  }

  implicit val RowAccessTraverse: Traverse[RowAccess] = new Traverse[RowAccess] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowAccess[A]): F[RowAccess[B]] =
      as foldV ((e: SQLException) => err(e).η[F], v => f(v) ∘ (value(_)), nul.η[F])
  }

  implicit def RowAccessShow[A: Show]: Show[RowAccess[A]] = new Show[RowAccess[A]] {
    def show(a: RowAccess[A]) =
      a foldV (
              e => ("row-error(" + e + ")").toList
            , a => ("row-value(" + a.shows + ")").toList
            , "row-null".toList
            )
  }

  implicit def RowAccessEqual[A: Equal]: Equal[RowAccess[A]] = new Equal[RowAccess[A]] {
    def equal(a1: RowAccess[A], a2: RowAccess[A]) =
      a1 fold (t => a2 fold (u => t === u, false), a2.isNull)
  }

  implicit def SQLValueZero[A: Zero]: Zero[RowAccess[A]] = zero(value(∅[A]))
}
