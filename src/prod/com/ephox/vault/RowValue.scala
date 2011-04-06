package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._

sealed trait RowValue[A] extends NewType[WLOG[Option[Either[SqlException, A]]]] {
  import PossiblyNull._

  import RowValue._

  def fold[X](sqlErr: SqlException => X, sqlValue: A => X, nul: => X): X =
    value.over match {
      case None           => nul
      case Some(Left(e))  => sqlErr(e)
      case Some(Right(a)) => sqlValue(a)
    }

  def isNull: Boolean = fold(_ => false, _ => false, true)
  def isNotNull: Boolean = !isNull
  def isError: Boolean = fold(_ => true, _ => false, false)
  def isNotError: Boolean = !isError
  def isValue: Boolean = fold(_ => false, _ => true, false)
  def isNotValue: Boolean = !isValue

  def getError: Option[SqlException] =
    fold(Some(_), _ => None, None)

  def getErrorOr(e: => SqlException): SqlException =
    getError getOrElse e

  def getValue: Option[A] =
    fold(_ => None, Some(_), None)

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getOrDie: A =
    fold(e => throw new VaultException(e), x => x, throw new VaultException("Unexpected database null."))

  def getSqlValue: Option[SqlValue[A]] =
    fold(e => Some(sqlError(e)), a => Some(sqlValue(a)), None)

  def getSqlValueOr(v: => SqlValue[A]): SqlValue[A] =
    getSqlValue getOrElse v

  def printStackTraceOr(v: A => Unit, nul: => Unit): Unit =
    fold(_ => (), v, nul)

  def map[B](f: A => B): RowValue[B] =
    fold(rowError(_), a => rowValue(f(a)), rowNull)

  def flatMap[B](f: A => RowValue[B]): RowValue[B] =
    fold(rowError(_), f, rowNull)

  def unifyNullWithMessage(message: String): SqlValue[A] =
    fold[SqlValue[A]](sqlError(_), sqlValue(_), sqlError(new SqlException(message))) setLog log

  def unifyNull: SqlValue[A] =
    unifyNullWithMessage("unify null")

  def possiblyNull: SqlValue[PossiblyNull[A]] =
    optionPossiblyNull(getSqlValue).sequence[SqlValue, A] setLog log

  def possiblyNullOr(d: => A): SqlValue[A] =
    possiblyNull map (_ | d)

  def toList: SqlValue[List[A]] =
    possiblyNull map (_.toList)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)

  /**
   * Lifts this value into a possibly null value. The following holds:
   *
   * forall r. r.liftPossiblyNull.isNotNull
   */
  def liftPossiblyNull: RowValue[PossiblyNull[A]] =
    possiblyNull.toRowValue

  /**
   * Return the log associated with this value.
   */
  def log: LOG =
    value.written

  /**
   * Sets the log to the given value.
   */
  def setLog(k: LOG): RowValue[A] = new RowValue[A] {
    val value =
      RowValue.this.value.over set k
  }

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG => LOG): RowValue[A] = new RowValue[A] {
    val value =
      RowValue.this.value.over set (k(RowValue.this.log))
  }

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: LOGV => LOGV): RowValue[A] =
    withLog(_ ∘ k)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LOGV): RowValue[A] =
    withLog(_ |+| e.η[LOGC])

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :->>(e: Option[Either[SqlException, A]] => LOGV): RowValue[A] =
    :+->(e(value.over))

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): RowValue[A] =
    withLog(e.η[LOGC] |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-:(e: Option[Either[SqlException, A]] => LOGV): RowValue[A] =
    <-+:(e(value.over))
/**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): RowValue[A] =
    withLog(_ |+| e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :+->>(e: Option[Either[SqlException, A]] => LOG): RowValue[A] =
    withLog(_ |+| e(value.over))

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): RowValue[A] =
    withLog(e |+| _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-+:(e: Option[Either[SqlException, A]] => LOG): RowValue[A] =
    <-++:(e(value.over))

  /**
   * Set the log to be empty.
   */
  def resetLog: RowValue[A] =
    withLog(_ => ∅[LOG])
}

object RowValue extends RowValues

trait RowValues {
  def rowError[A](e: SqlException): RowValue[A] = new RowValue[A] {
    val value = (Some(Left(e)): Option[Either[SqlException, A]]).η[WLOG]
  }

  def rowValue[A](a: A): RowValue[A] = new RowValue[A] {
    val value = (Some(Right(a)): Option[Either[SqlException, A]]).η[WLOG]
  }

  def rowNull[A]: RowValue[A] = new RowValue[A] {
    val value = (None: Option[Either[SqlException, A]]).η[WLOG]
  }

  def tryRowValue[A](a: => A): RowValue[A] =
    trySqlValue(a).toRowValue

  implicit val RowValueInjective = Injective[RowValue]

  implicit val RowValueFunctor: Functor[RowValue] = new Functor[RowValue] {
    def fmap[A, B](r: RowValue[A], f: A => B) =
      r map f
  }

  implicit val RowValuePure: Pure[RowValue] = new Pure[RowValue] {
    def pure[A](a: => A) =
      rowValue(a)
  }

  implicit val RowValueBind: Bind[RowValue] = new Bind[RowValue] {
    def bind[A, B](a: RowValue[A], f: A => RowValue[B]) =
      a fold (rowError(_), f, rowNull)
  }

  implicit val RowValueEach: Each[RowValue] = new Each[RowValue] {
    def each[A](e: RowValue[A], f: A => Unit) =
      e fold (_ => (), f, ())
  }

  implicit val RowAccessIndex: Index[RowValue] = new Index[RowValue] {
    def index[A](a: RowValue[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val RowAccessLength: Length[RowValue] = new Length[RowValue] {
    def len[A](a: RowValue[A]) =
      a fold (_ => 0, _ => 1, 0)
  }

  implicit val RowAccessFoldable: Foldable[RowValue] = new Foldable[RowValue] {
    override def foldLeft[A, B](e: RowValue[A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _), b)

    override def foldRight[A, B](e: RowValue[A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b), b)
  }

  implicit val RowAccessTraverse: Traverse[RowValue] = new Traverse[RowValue] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowValue[A]): F[RowValue[B]] =
      as fold ((e: SqlException) => rowError(e).η[F], v => f(v) ∘ (rowValue(_)), rowNull.η[F])
  }

  implicit def RowAccessShow[A: Show]: Show[RowValue[A]] = new Show[RowValue[A]] {
    def show(a: RowValue[A]) =
      a fold (
              e => ("row-error(" + e + ")")
            , a => ("row-value(" + a.shows + ")")
            , "row-null"
            ) toList
  }

  implicit def RowAccessEqual[A: Equal]: Equal[RowValue[A]] = new Equal[RowValue[A]] {
    def equal(a1: RowValue[A], a2: RowValue[A]) =
      a1 fold (
        _ => a2.isError
      , t => a2 fold (_ => false, t === _, false)
      , a2.isNull)
  }

  implicit def RowAccessZero[A: Zero]: Zero[RowValue[A]] = zero(rowValue(∅[A]))
}
