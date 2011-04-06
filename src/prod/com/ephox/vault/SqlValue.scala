package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import RowValue._

sealed trait SqlValue[A] extends NewType[WLOG[Either[SqlException, A]]] {
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

  def toRowValue: RowValue[A] =
    fold[RowValue[A]](rowError, rowValue(_)) setLog log

  def printStackTraceOr(f: A => Unit): Unit =
    fold(_.printStackTrace, f)

  def map[B](f: A => B): SqlValue[B] = new SqlValue[B] {
    val value =
      SqlValue.this.value map (_ map f)
  }

  def flatMap[B](f: A => SqlValue[B]): SqlValue[B] = new SqlValue[B] {
    val value =
      SqlValue.this.value flatMap (_.right.flatMap(f(_).toEither).η[WLOG])
  }

  /**
   * Return the log associated with this value.
   */
  def log: LOG = value.written

  /**
   * Sets the log to the given value.
   */
  def setLog(k: LOG): SqlValue[A] = new SqlValue[A] {
    val value =
      SqlValue.this.value.over set k
  }

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG => LOG): SqlValue[A] = new SqlValue[A] {
    val value =
      SqlValue.this.value.over set (k(SqlValue.this.log))
  }

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: LOGV => LOGV): SqlValue[A] =
    withLog(_ ∘ k)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LOGV): SqlValue[A] =
    withLog(_ |+| e.η[LOGC])

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :->>(e: Either[SqlException, A] => LOGV): SqlValue[A] =
    :+->(e(value.over))

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): SqlValue[A] =
    withLog(e.η[LOGC] |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-:(e: Either[SqlException, A] => LOGV): SqlValue[A] =
    <-+:(e(value.over))
/**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): SqlValue[A] =
    withLog(_ |+| e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :+->>(e: Either[SqlException, A] => LOG): SqlValue[A] =
    withLog(_ |+| e(value.over))

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): SqlValue[A] =
    withLog(e |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-+:(e: Either[SqlException, A] => LOG): SqlValue[A] =
    <-++:(e(value.over))

  /**
   * Set the log to be empty.
   */
  def resetLog: SqlValue[A] =
    withLog(_ => ∅[LOG])
}

object SqlValue extends SqlValues

trait SqlValues {
  type SqlException = java.sql.SQLException

  type LOGV = String
  type LOGC[V] = IndSeq[V]
  type LOG = LOGC[LOGV]
  type WLOG[A] = Writer[LOG, A]

  def sqlError[A](e: SqlException): SqlValue[A] = new SqlValue[A] {
    val value = (Left(e): Either[SqlException, A]).η[WLOG]
  }

  def sqlValue[A](v: A): SqlValue[A] = new SqlValue[A] {
    val value = (Right(v): Either[SqlException, A]).η[WLOG]
  }

  def trySqlValue[A](a: => A): SqlValue[A] =
    try {
      sqlValue[A](a)
    } catch {
      case e: SqlException => sqlError(e)
      case e               => throw e
    }

  def withSqlResource[T, R, L](
                          value: => T
                        , evaluate: T => SqlValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SqlValue[R] =
    withResource(value, evaluate, {
      case e: SqlException => sqlError(e)
      case e               => throw e
    }, whenClosing)

  implicit val SqlValueInjective = Injective[SqlValue]

  implicit val SqlValueFunctor: Functor[SqlValue] = new Functor[SqlValue] {
    def fmap[A, B](r: SqlValue[A], f: A => B) =
      r map f
  }

  implicit val SqlValuePure: Pure[SqlValue] = new Pure[SqlValue] {
    def pure[A](a: => A) = sqlValue(a)
  }

  implicit val SqlValueBind: Bind[SqlValue] = new Bind[SqlValue] {
    def bind[A, B](a: SqlValue[A], f: A => SqlValue[B]) = a flatMap f
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
      as fold ((e: SqlException) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
  }


  implicit val SqlValuePlus: Plus[SqlValue] = new Plus[SqlValue] {
    def plus[A](a1: SqlValue[A], a2: => SqlValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit val SqlValueEmpty: Empty[SqlValue] = new Empty[SqlValue] {
    def empty[A] = sqlError(new SqlException)
  }

  implicit def SqlValueShow[A: Show]: Show[SqlValue[A]] = new Show[SqlValue[A]] {
    def show(a: SqlValue[A]) =
      a fold(
              e => ("error(" + e + ")")
            , a => ("value(" + a.shows + ")")
            ) toList
  }

  implicit def SqlValueEqual[A: Equal]: Equal[SqlValue[A]] = {
    implicit val EqualSqlException: Equal[SqlException] = equalA
    Equal.EitherEqual[SqlException, A] ∙ (_.toEither)
  }

  implicit def SqlValueOrder[A: Order]: Order[SqlValue[A]] = {
    implicit val OrderSqlException: Order[SqlException] = new Order[SqlException] {
      def order(a1: SqlException, a2: SqlException) = EQ
    }
    Order.EitherOrder[SqlException, A] ∙ (_.toEither)
  }

  implicit def SqlValueZero[A: Zero]: Zero[SqlValue[A]] =
    zero(sqlValue(∅[A]))
}
