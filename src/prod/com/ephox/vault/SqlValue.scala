package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import RowValue._
import SqlExceptionContext._
import java.sql.PreparedStatement

sealed trait SqlValue[A] {
  protected val value: WLOG[Either[SqlExceptionContext, A]]

  def fold[X](err: SqlExceptionContext => X, v: A => X) =
    value.over.fold(err, v)

  def isError: Boolean =
    fold(_ => true, _ => false)

  def isValue: Boolean =
    !isError

  def getError: Option[SqlExceptionContext] =
    fold(Some(_), _ => None)

  def getErrorOr(e: => SqlExceptionContext): SqlExceptionContext =
    getError getOrElse e

  def mapError(k: SqlExceptionContext => SqlExceptionContext): SqlValue[A] =
    fold(e => sqlError(k(e)) setLog log, _ => this)

  def getValue: Option[A] =
    fold(_ => None, Some(_))

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getOrDie: A =
    fold(e => throw new VaultException(e.detail, e.sqlException), x => x)

  def toEither:Either[SqlExceptionContext, A] =
    fold(Left(_), Right(_))

  def toValidation: Validation[SqlExceptionContext, A] =
    fold(failure(_), success(_))

  def toRowValue: RowValue[A] =
    fold[RowValue[A]](rowError, rowValue(_)) setLog log

  def printStackTraceOr(f: A => Unit): Unit =
    fold(_.sqlException.printStackTrace, f)

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
    withLog(_ map k)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LOGV): SqlValue[A] =
    withLog(_ |+| e.η[LOGC])

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): SqlValue[A] =
    withLog(e.η[LOGC] |+| _)

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): SqlValue[A] =
    withLog(_ |+| e)

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): SqlValue[A] =
    withLog(e |+| _)

  /**
   * Set the log to be empty.
   */
  def resetLog: SqlValue[A] =
    withLog(_ => ∅[LOG])
}

object SqlValue extends SqlValues

trait SqlValues {
  type LOGV = String
  type LOGC[V] = IndSeq[V]
  type LOG = LOGC[LOGV]
  type WLOG[A] = Writer[LOG, A]

  def sqlError[A](e: SqlExceptionContext): SqlValue[A] = new SqlValue[A] {
    val value = (Left(e): Either[SqlExceptionContext, A]).η[WLOG]
  }

  def sqlValue[A](v: A): SqlValue[A] = new SqlValue[A] {
    val value = (Right(v): Either[SqlExceptionContext, A]).η[WLOG]
  }

  def trySqlValue[A](a: => A): SqlValue[A] =
    try {
      sqlValue[A](a)
    } catch {
      case e: SqlException => sqlError(sqlExceptionContext(e))
      case e               => throw e
    }

  def withSqlResource[T, R, L](
                          value: => T
                        , evaluate: T => SqlValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SqlValue[R] =
    withResource(value, evaluate, {
      case e: SqlException => sqlError(sqlExceptionContext(e))
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
      as fold ((e: SqlExceptionContext) => sqlError(e).η[F], v => f(v) ∘ (sqlValue(_)))
  }


  implicit val SqlValuePlus: Plus[SqlValue] = new Plus[SqlValue] {
    def plus[A](a1: SqlValue[A], a2: => SqlValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)
  }

  implicit val SqlValueEmpty: Empty[SqlValue] = new Empty[SqlValue] {
    def empty[A] = sqlError(sqlExceptionContext(new SqlException))
  }

  implicit def SqlValueShow[A: Show]: Show[SqlValue[A]] = new Show[SqlValue[A]] {
    def show(a: SqlValue[A]) =
      a fold(
              e => ("error(" + e + ")")
            , a => ("value(" + a.shows + ")")
            ) toList
  }

  implicit def SqlValueEqual[A: Equal]: Equal[SqlValue[A]] = {
    implicit val EqualSqlExceptionContext: Equal[SqlExceptionContext] = new Equal[SqlExceptionContext] {
      def equal(a1: SqlExceptionContext, a2: SqlExceptionContext) = true
    }
    Equal.EitherEqual[SqlExceptionContext, A] ∙ (_.toEither)
  }

  implicit def SqlValueOrder[A: Order]: Order[SqlValue[A]] = {
    implicit val OrderSqlExceptionContext: Order[SqlExceptionContext] = new Order[SqlExceptionContext] {
      def order(a1: SqlExceptionContext, a2: SqlExceptionContext) = EQ
    }
    Order.EitherOrder[SqlExceptionContext, A] ∙ (_.toEither)
  }

  implicit def SqlValueZero[A: Zero]: Zero[SqlValue[A]] =
    zero(sqlValue(∅[A]))
}
