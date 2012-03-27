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

  @annotation.tailrec
  final def loop[X](e: SqlExceptionContext => X, v: A => Either[X, SqlValue[A]]): X =
    if (isError)
      e(getError.get)
    else
      v(getValue.get) match {
        case Left(x) => x
        case Right(r) => r.loop(e, v)
      }

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
    fold(Failure(_), Success(_))

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
      SqlValue.this.value flatMap (_.right.flatMap(f(_).toEither).point[WLOG])
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
    withLog(_ |+| e.point[LOGC])

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): SqlValue[A] =
    withLog(e.point[LOGC] |+| _)

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
    withLog(_ => Monoid[LOG].zero)
}

object SqlValue extends SqlValues

trait SqlValues {
  type LOGV = String
  type LOGC[V] = DList[V]
  type LOG = LOGC[LOGV]
  type WLOG[A] = Writer[LOG, A]

  import Resource._

  def sqlError[A](e: SqlExceptionContext): SqlValue[A] = new SqlValue[A] {
    val value = (Left(e): Either[SqlExceptionContext, A]).point[WLOG]
  }

  def sqlValue[A](v: A): SqlValue[A] = new SqlValue[A] {
    val value = (Right(v): Either[SqlExceptionContext, A]).point[WLOG]
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

  implicit val SqlValueInstance: MonadPlus[SqlValue] with Traverse[SqlValue] with Length[SqlValue] with Index[SqlValue] with Each[SqlValue] = new MonadPlus[SqlValue] with Traverse[SqlValue] with Length[SqlValue] with Index[SqlValue] with Each[SqlValue] {
    def empty[A] = sqlError(sqlExceptionContext(new SqlException))

    def plus[A](a1: SqlValue[A], a2: => SqlValue[A]) =
      a1 fold (_ => a2 fold (_ => a1, _ => a2), _ => a1)

    def bind[A, B](a: SqlValue[A])(f: A => SqlValue[B]) =
      a flatMap f

    def point[A](a: => A) =
      sqlValue(a)

    def traverseImpl[F[_] : Applicative, A, B](as: SqlValue[A])(f: A => F[B]): F[SqlValue[B]] =
      as fold ((e: SqlExceptionContext) => sqlError(e).point[F], v => f(v) ∘ (sqlValue(_)))

    def length[A](a: SqlValue[A]) =
      a fold(_ => 0, _ => 1)

    def index[A](a: SqlValue[A], i: Int) =
      a.getValue filter (_ => i == 0)

    def each[A](e: SqlValue[A])(f: A => Unit) =
      e fold (_ => (), f)
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
    Equal.equalBy(_.toEither)
  }

  implicit def SqlValueOrder[A: Order]: Order[SqlValue[A]] = {
    implicit val OrderSqlExceptionContext: Order[SqlExceptionContext] = new Order[SqlExceptionContext] {
      def order(a1: SqlExceptionContext, a2: SqlExceptionContext) = Ordering.EQ
    }
    Order.orderBy(_.toEither)
  }

}
