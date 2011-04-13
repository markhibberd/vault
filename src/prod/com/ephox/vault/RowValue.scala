package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import RowValue._
import SqlExceptionContext._
import scala.Option._

sealed trait RowValue[A] {
  protected val value: WLOG[Either[Option[NullMsg], Either[SqlExceptionContext, A]]]

  import PossiblyNull._

  def fold[X](sqlErr: SqlExceptionContext => X, sqlValue: A => X, nul: Option[NullMsg] => X): X =
    value.over match {
      case Left(n)         => nul(n)
      case Right(Left(e))  => sqlErr(e)
      case Right(Right(a)) => sqlValue(a)
    }

  def foldOrNullMsg[X](defaultNullMsg: => NullMsg)(sqlErr: SqlExceptionContext => X, sqlValue: A => X, nul: NullMsg => X) =
    fold(sqlErr, sqlValue, m => nul(m getOrElse defaultNullMsg))

  def isNull: Boolean = fold(_ => false, _ => false, _ => true)
  def isNotNull: Boolean = !isNull
  def isError: Boolean = fold(_ => true, _ => false, _ => false)
  def isNotError: Boolean = !isError
  def isValue: Boolean = fold(_ => false, _ => true, _ => false)
  def isNotValue: Boolean = !isValue

  def getError: Option[SqlExceptionContext] =
    fold(Some(_), _ => None, _ => None)

  def getErrorOr(e: => SqlExceptionContext): SqlExceptionContext =
    getError getOrElse e

  def mapError(k: SqlExceptionContext => SqlExceptionContext): RowValue[A] =
    fold(e => rowError(k(e)) setLog log, _ => this, _ => this)

  def getValue: Option[A] =
    fold(_ => None, Some(_), _ => None)

  def getValueOr(v: => A): A =
    getValue getOrElse v

  def getOrDie: A =
    fold(e => throw new VaultException(e.sqlException), x => x, n => throw new VaultException("Unexpected database null: " + n.getOrElse("")))

  def getSqlValue: Option[SqlValue[A]] =
    fold(e => Some(sqlError(e) setLog log), a => Some(sqlValue(a) setLog log), _ => None)

  def getSqlValueOr(v: => SqlValue[A]): SqlValue[A] =
    getSqlValue getOrElse v

  def getNullMsg: Option[NullMsg] =
    fold(_ => None, _ => None, x => x)

  def getNullMsgOr(o: => NullMsg): NullMsg =
    getNullMsg getOrElse o

  def printStackTraceOr(v: A => Unit, nul: Option[NullMsg] => Unit): Unit =
    fold(_ => (), v, nul)

  def map[B](f: A => B): RowValue[B] =
    fold(rowError(_), a => rowValue(f(a)), rowNullPossibleMsg(_))

  def flatMap[B](f: A => RowValue[B]): RowValue[B] =
    fold(rowError(_), f, rowNullPossibleMsg(_))

  def unifyNullWithMessage(message: String): SqlValue[A] =
    fold[SqlValue[A]](sqlError(_), sqlValue(_), _ => sqlError(sqlExceptionContext(new SqlException(message)))) setLog log

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
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): RowValue[A] =
    withLog(e.η[LOGC] |+| _)

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): RowValue[A] =
    withLog(_ |+| e)

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): RowValue[A] =
    withLog(e |+| _)

  /**
   * Set the log to be empty.
   */
  def resetLog: RowValue[A] =
    withLog(_ => ∅[LOG])
}

object RowValue extends RowValues

trait RowValues {
  type NullMsg = String

  def rowError[A](e: SqlExceptionContext): RowValue[A] = new RowValue[A] {
    val value = (Right(Left(e)): Either[Option[NullMsg], Either[SqlExceptionContext, A]]).η[WLOG]
  }

  def rowValue[A](a: A): RowValue[A] = new RowValue[A] {
    val value = (Right(Right(a)): Either[Option[NullMsg], Either[SqlExceptionContext, A]]).η[WLOG]
  }

  def rowNullPossibleMsg[A](note: Option[NullMsg]): RowValue[A] = new RowValue[A] {
    val value = (Left(note): Either[Option[NullMsg], Either[SqlExceptionContext, A]]).η[WLOG]
  }

  def rowNull[A]: RowValue[A] = new RowValue[A] {
    val value = (Left(None): Either[Option[NullMsg], Either[SqlExceptionContext, A]]).η[WLOG]
  }

  def rowNullMsg[A](note: NullMsg): RowValue[A] = new RowValue[A] {
    val value = (Left(Some(note)): Either[Option[NullMsg], Either[SqlExceptionContext, A]]).η[WLOG]
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
      a fold (rowError(_), f, rowNullPossibleMsg(_))
  }

  implicit val RowValueEach: Each[RowValue] = new Each[RowValue] {
    def each[A](e: RowValue[A], f: A => Unit) =
      e fold (_ => (), f, _ => ())
  }

  implicit val RowValueIndex: Index[RowValue] = new Index[RowValue] {
    def index[A](a: RowValue[A], i: Int) = a.getValue filter (_ => i == 0)
  }

  implicit val RowValueLength: Length[RowValue] = new Length[RowValue] {
    def len[A](a: RowValue[A]) =
      a fold (_ => 0, _ => 1, _ => 0)
  }

  implicit val RowValueFoldable: Foldable[RowValue] = new Foldable[RowValue] {
    override def foldLeft[A, B](e: RowValue[A], b: B, f: (B, A) => B) =
      e fold (_ => b, f(b, _), _ => b)

    override def foldRight[A, B](e: RowValue[A], b: => B, f: (A, => B) => B) =
      e fold (_ => b, f(_, b), _ => b)
  }

  implicit val RowValueTraverse: Traverse[RowValue] = new Traverse[RowValue] {
    def traverse[F[_] : Applicative, A, B](f: A => F[B], as: RowValue[A]): F[RowValue[B]] =
      as fold ((e: SqlExceptionContext) => rowError(e).η[F], v => f(v) ∘ (rowValue(_)), _ => rowNull.η[F])
  }

  implicit def RowValueShow[A: Show]: Show[RowValue[A]] = new Show[RowValue[A]] {
    def show(a: RowValue[A]) =
      a fold (
              e => ("row-error(" + e + ")")
            , a => ("row-value(" + a.shows + ")")
            , o => "row-null[" + o.getOrElse("") + "]"
            ) toList
  }

  implicit def RowValueEqual[A: Equal]: Equal[RowValue[A]] = new Equal[RowValue[A]] {
    def equal(a1: RowValue[A], a2: RowValue[A]) =
      a1 fold (
        _ => a2.isError
      , t => a2 fold (_ => false, t === _, _ => false)
      , _ => a2.isNull)
  }

  implicit def RowValueZero[A: Zero]: Zero[RowValue[A]] = zero(rowValue(∅[A]))
}
