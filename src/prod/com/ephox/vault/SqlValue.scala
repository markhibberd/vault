package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait SqlValue[L, A] extends NewType[Logger[L, Either[SqlException, A]]] {
  def fold[X](err: SqlException => X, value: A => X): X


  def isError: Boolean =
    // fold(_ => true, _ => false)
    error("todo")

  def isValue: Boolean =
    // !isError
    error("todo")

  def getError: Option[SqlException] =
    // fold(Some(_), _ => None)
    error("todo")

  def getErrorOr(e: => SqlException): SqlException =
    // getError getOrElse e
    error("todo")

  def getValue: Option[A] =
    // fold(_ => None, Some(_))
    error("todo")

  def getValueOr(v: => A): A =
    // getValue getOrElse v
    error("todo")

  def toEither:Either[SqlException, A] =
    // fold(Left(_), Right(_))
    error("todo")

  def toValidation: Validation[SqlException, A] =
    // fold(failure(_), success(_))
    error("todo")

  def toRowAccess: RowValue[L, A] =
    // fold(rowError, rowValue(_))
    error("todo")

  def printStackTraceOr(f: A => Unit): Unit =
    // fold(_.printStackTrace, f)
    error("todo")

  def map[B](f: A => B): SqlValue[L, B] = /* new SqlValue[B] {
    def fold[X](err: SqlException => X, value: B => X) =
      SqlValue.this.fold(err, value compose f)
  } */
    error("todo")

  def flatMap[B](f: A => SqlValue[L, B]): SqlValue[L, B] = /* new SqlValue[B] {
    def fold[X](err: SqlException => X, value: B => X) =
      SqlValue.this.fold(err, a => f(a) fold (err, value))
  } */
    error("todo")
}

trait SqlValues {
  type SqlException = java.sql.SQLException

  def sqlError[L, A](e: SqlException): SqlValue[L, A] = /* new SqlValue[A] {
    def fold[X](err: SqlException => X, value: A => X) = err(e)
  } */
    error("todo")

  def sqlValue[L, A](v: A): SqlValue[L, A] = /* new SqlValue[A] {
    def fold[X](err: SqlException => X, va: A => X) = va(v)
  } */
    error("todo")

  def trySqlValue[L, A](a: => A): SqlValue[L, A] =
    /*
    try {
      a.η[SqlValue]
    } catch {
      case e: SqlException => sqlError(e)
      case e               => throw e
    }
    */
    error("todo")

//  implicit val SqlValueInjective = Injective[SqlValue]
//
//  implicit val SqlValueFunctor: Functor[SqlValue] = /* new Functor[SqlValue] {
//    def fmap[A, B](r: SqlValue[A], f: A => B) =
//      r fold (sqlError(_), a => sqlValue(f(a)))
//  } */
//    error("todo")
//
//  implicit val SqlValuePure: Pure[SqlValue] = /* new Pure[SqlValue] {
//    def pure[A](a: => A) =
//      sqlValue(a)
//  } */
//    error("todo")
//
//  implicit val SqlValueApply: Apply[SqlValue] = /* new Apply[SqlValue] {
//    def apply[A, B](f: SqlValue[A => B], a: SqlValue[A]) =
//      f fold (sqlError(_), ff => a fold (sqlError(_), aa => sqlValue(ff(aa))))
//  } */
//    error("todo")
//
//  implicit val SqlValueBind: Bind[SqlValue] = /* new Bind[SqlValue] {
//    def bind[A, B](a: SqlValue[A], f: A => SqlValue[B]) =
//      a fold (sqlError(_), f)
//  } */
//    error("todo")
//
//  implicit val SqlValueEach: Each[SqlValue] = /* new Each[SqlValue] {
//    def each[A](e: SqlValue[A], f: A => Unit) =
//      e fold (_ => (), f)
//  } */
//    error("todo")
//
//  implicit val SqlValueIndex: Index[SqlValue] = /* new Index[SqlValue] {
//    def index[A](a: SqlValue[A], i: Int) = a.getValue filter (_ => i == 0)
//  } */
//    error("todo")
//
//  implicit val SqlValueLength: Length[SqlValue] = /* new Length[SqlValue] {
//    def len[A](a: SqlValue[A]) =
//      a fold(_ => 0, _ => 1)
//  } */
//    error("todo")
//
//  implicit val SqlValueFoldable: Foldable[SqlValue] = /* new Foldable[SqlValue] {
//    override def foldLeft[A, B](e: SqlValue[A], b: B, f: (B, A) => B) =
//      e fold (_ => b, f(b, _))
//
//    override def foldRight[A, B](e: SqlValue[A], b: => B, f: (A, => B) => B) =
//      e fold (_ => b, f(_, b))
//  } */
//    error("todo")
//
//  implicit val SqlValueTraverse: Traverse[SqlValue] = /* new Traverse[SqlValue] {
//    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: SqlValue[A]): F[SqlValue[B]] =
//      as fold ((e: SqlException) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
//  } */
//    error("todo")
//
//  implicit val SqlValuePlus: Plus[SqlValue] = /* new Plus[SqlValue] {
//    def plus[A](a1: SqlValue[A], a2: => SqlValue[A]) =
//      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
//  } */
//    error("todo")
//
//  implicit val SqlValueEmpty: Empty[SqlValue] = /* new Empty[SqlValue] {
//    def empty[A] = sqlError(new SqlException)
//  } */
//    error("todo")
//
//  implicit def SqlValueShow[A: Show]: Show[SqlValue[A]] = /* new Show[SqlValue[A]] {
//    def show(a: SqlValue[A]) =
//      a fold(
//              e => ("error(" + e + ")").toList
//            , a => ("value(" + a.shows + ")").toList
//            )
//  } */
//    error("todo")
//
//  implicit def SqlValueEqual[A: Equal]: Equal[SqlValue[A]] = /* {
//    implicit val EqualSqlException: Equal[SqlException] = equalA
//    Equal.EitherEqual[SqlException, A] ∙ (_.toEither)
//  } */
//    error("todo")
//
//  implicit def SqlValueOrder[A: Order]: Order[SqlValue[A]] = /* {
//    implicit val OrderSqlException: Order[SqlException] = new Order[SqlException] {
//      def order(a1: SqlException, a2: SqlException) = EQ
//    }
//    Order.EitherOrder[SqlException, A] ∙ (_.toEither)
//  } */
//    error("todo")
//
//  implicit def SqlValueZero[A: Zero]: Zero[SqlValue[A]] =
//    // zero(sqlValue(∅[A]))
//    error("todo")
}
