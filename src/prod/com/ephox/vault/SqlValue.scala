package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait SqlValue[L, A] extends NewType[Logger[L, Either[SqlException, A]]] {
  def fold[X](err: SqlException => X, v: A => X) =
    value.over.fold(err, v)

  def isError: Boolean =
    fold(_ => true, _ => false)

  def isValue: Boolean =
    !isError

  def getError: Option[SqlException] =
    fold(Some(_), _ => None)

  def getErrorOr(e: => SqlException): SqlException =
    getError getOrElse e

  def getValue: Option[A] =
    fold(_ => None, Some(_))

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getOrDie: A =
    fold(e => throw new VaultException(e), x => x)

  def toEither:Either[SqlException, A] =
    fold(Left(_), Right(_))

  def toValidation: Validation[SqlException, A] =
    fold(failure(_), success(_))

  def toRowValue: RowValue[L, A] =
    fold(rowError, rowValue(_))

  def printStackTraceOr(f: A => Unit): Unit =
    fold(_.printStackTrace, f)

  def map[B](f: A => B): SqlValue[L, B] = new SqlValue[L, B] {
    val value =
      SqlValue.this.value map (_ map f)
  }

  def flatMap[B](f: A => SqlValue[L, B]): SqlValue[L, B] = new SqlValue[L, B] {
    val value =
      SqlValue.this.value flatMap (e => (e.right flatMap (f(_).toEither)).logger[L])
  }
}

trait SqlValues {
  type SqlException = java.sql.SQLException

  def sqlError[L, A](e: SqlException): SqlValue[L, A] = new SqlValue[L, A] {
    val value = (Left(e): Either[SqlException, A]).logger[L]
  }

  def sqlValue[L, A](v: A): SqlValue[L, A] = new SqlValue[L, A] {
    val value = (Right(v): Either[SqlException, A]).logger[L]
  }

  def trySqlValue[L, A](a: => A): SqlValue[L, A] =
    try {
      sqlValue[L, A](a)
    } catch {
      case e: SqlException => sqlError(e)
      case e               => throw e
    }

  implicit def SqlValueInjective[L] = Injective[({type λ[α]= SqlValue[L, α]})#λ]

  implicit def SqlValueFunctor[L]: Functor[({type λ[α]= SqlValue[L, α]})#λ] = new Functor[({type λ[α]= SqlValue[L, α]})#λ] {
    def fmap[A, B](r: SqlValue[L, A], f: A => B) =
      r map f
  }

  implicit def SqlValuePure[L]: Pure[({type λ[α]= SqlValue[L, α]})#λ] = new Pure[({type λ[α]= SqlValue[L, α]})#λ] {
    def pure[A](a: => A) =
      sqlValue(a)
  }

  implicit def SqlValueApply[L]: Apply[({type λ[α]= SqlValue[L, α]})#λ] = new Apply[({type λ[α]= SqlValue[L, α]})#λ] {
    def apply[A, B](f: SqlValue[L, A => B], a: SqlValue[L, A]) =
      f fold (sqlError(_), ff => a fold (sqlError(_), aa => sqlValue(ff(aa))))
  }

  implicit def SqlValueApplicative[L]: Applicative[({type λ[α]= SqlValue[L, α]})#λ] = Applicative.applicative[({type λ[α]= SqlValue[L, α]})#λ]


  implicit def SqlValueBind[L]: Bind[({type λ[α]= SqlValue[L, α]})#λ] = new Bind[({type λ[α]= SqlValue[L, α]})#λ] {
    def bind[A, B](a: SqlValue[L, A], f: A => SqlValue[L, B]) =
      a fold (sqlError(_), f)
  }

  implicit def SqlValueMonad[L]: Monad[({type λ[α]= SqlValue[L, α]})#λ] = Monad.monad[({type λ[α]= SqlValue[L, α]})#λ]

  implicit def SqlValueEach[L]: Each[({type λ[α]= SqlValue[L, α]})#λ] = new Each[({type λ[α]= SqlValue[L, α]})#λ] {
    def each[A](e: SqlValue[L, A], f: A => Unit) =
      e fold (_ => (), f)
  }

  implicit def SqlValueIndex[L]: Index[({type λ[α]= SqlValue[L, α]})#λ] = new Index[({type λ[α]= SqlValue[L, α]})#λ] {
    def index[A](a: SqlValue[L, A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit def SqlValueLength[L]: Length[({type λ[α]= SqlValue[L, α]})#λ] = new Length[({type λ[α]= SqlValue[L, α]})#λ] {
    def len[A](a: SqlValue[L, A]) =
      a fold(_ => 0, _ => 1)
  }

  implicit def SqlValueFoldable[L]: Foldable[({type λ[α]= SqlValue[L, α]})#λ] = new Foldable[({type λ[α]= SqlValue[L, α]})#λ] {
    override def foldLeft[A, B](e: SqlValue[L, A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _))

    override def foldRight[A, B](e: SqlValue[L, A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b))
  }

  implicit def SqlValueTraverse[L]: Traverse[({type λ[α]= SqlValue[L, α]})#λ] = new Traverse[({type λ[α]= SqlValue[L, α]})#λ] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: SqlValue[L, A]): F[SqlValue[L, B]] =
      as fold ((e: SqlException) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
  }


  implicit def SqlValuePlus[L]: Plus[({type λ[α]= SqlValue[L, α]})#λ] = new Plus[({type λ[α]= SqlValue[L, α]})#λ] {
    def plus[A](a1: SqlValue[L, A], a2: => SqlValue[L, A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit def SqlValueEmpty[L]: Empty[({type λ[α]= SqlValue[L, α]})#λ] = new Empty[({type λ[α]= SqlValue[L, α]})#λ] {
    def empty[A] = sqlError(new SqlException)
  }

  implicit def SqlValueShow[L, A: Show]: Show[SqlValue[L, A]] = new Show[SqlValue[L, A]] {
    def show(a: SqlValue[L, A]) =
      a fold(
              e => ("error(" + e + ")").toList
            , a => ("value(" + a.shows + ")").toList
            )
  }

  implicit def SqlValueEqual[L, A: Equal]: Equal[SqlValue[L, A]] = {
    implicit val EqualSqlException: Equal[SqlException] = equalA
    Equal.EitherEqual[SqlException, A] ∙ (_.toEither)
  }

  implicit def SqlValueOrder[L, A: Order]: Order[SqlValue[L, A]] = {
    implicit val OrderSqlException: Order[SqlException] = new Order[SqlException] {
      def order(a1: SqlException, a2: SqlException) = EQ
    }
    Order.EitherOrder[SqlException, A] ∙ (_.toEither)
  }

  implicit def SqlValueZero[L, A: Zero]: Zero[SqlValue[L, A]] =
    zero(sqlValue(∅[A]))
}
