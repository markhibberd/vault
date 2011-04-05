package com.ephox.vault

import scalaz._, Scalaz._

sealed trait RowAccess[A] {
  val access: Row => RowValue[A]
  import SqlValue._
  import SqlAccess._
  import RowValue._
  import RowConnect._
  import RowQueryConnect._
  import PreparedStatementW._

  def -|>[T](iter: IterV[A, T]): RowQueryConnect[IterV[A, T]] =
    rowQueryConnect(query => rowConnect(c => try {
      val st = c prepareStatement query.sql
      st.set(query.bindings:_*)
      try {
        val r = st.executeQuery
        try {
          Row.resultSetRow(r).iterate[A, T](this)(iter)
        } finally {
          r.close
        }
      } finally {
        st.close
      }
    } catch {
      case e: SqlException => rowError(e)
      case x               => throw x
    }))

  def -||>[T](iter: IterV[A, T]): RowQueryConnect[T] =
    -|>(iter) map (_.run)

  def map[B](f: A => B): RowAccess[B] = new RowAccess[B] {
    val access = (r: Row) => RowAccess.this.access(r) map f
  }

  def flatMap[B](f: A => RowAccess[B]): RowAccess[B] = new RowAccess[B] {
    val access = (r: Row) => RowAccess.this.access(r) flatMap (a => f(a).access(r))
  }

  def mapRowValue[B](f: RowValue[A] => RowValue[B]): RowAccess[B] = new RowAccess[B] {
    val access = (r: Row) => f(RowAccess.this.access(r))
  }

  def unifyNullWithMessage(message: String): SqlAccess[A] =
    sqlAccess((r: Row) => RowAccess.this.access(r) unifyNullWithMessage message)

  def unifyNull: SqlAccess[A] =
    sqlAccess((r: Row) => RowAccess.this.access(r).unifyNull)

  def possiblyNull: SqlAccess[PossiblyNull[A]] =
    sqlAccess(access(_).possiblyNull)

  def possiblyNullOr(d: => A): SqlAccess[A] =
    possiblyNull map (_ | d)

  def toList: SqlAccess[List[A]] =
    possiblyNull.map(_.toList)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)

  /**
   * Lifts this value into a possibly null value. The following holds:
   *
   * forall r s. r.liftPossiblyNull.access(s).isNotNull
   */
  def liftPossiblyNull: RowAccess[PossiblyNull[A]] =
    possiblyNull.toRowAccess

  /**
   * Return the log associated with this value.
   */
  def log: Row => LOG = access(_).log

  /**
   * Sets the log to the given value.
   */
  def setLog(k: LOG): RowAccess[A] =
    mapRowValue(_ setLog k)

  /**
   * Transform the log by the given function.
   */
  def withLog(k: LOG => LOG): RowAccess[A] =
    mapRowValue(_ withLog k)

  /**
   * Transform each log value by the given function.
   */
  def withEachLog(k: LOGV => LOGV): RowAccess[A] =
    mapRowValue(_ withEachLog k)

  /**
   * Append the given value to the current log.
   */
  def :+->(e: LOGV): RowAccess[A] =
    mapRowValue(_ :+-> e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :->>(e: Option[Either[SqlException, A]] => LOGV): RowAccess[A] =
    mapRowValue(_ :->> e)

  /**
   * Prepend the given value to the current log.
   */
  def <-+:(e: LOGV): RowAccess[A] =
    mapRowValue(e <-+: _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-:(e: Option[Either[SqlException, A]] => LOGV): RowAccess[A] =
    mapRowValue(e <<-: _)

  /**
   * Append the given value to the current log.
   */
  def :++->(e: LOG): RowAccess[A] =
    mapRowValue(_ :++-> e)

  /**
   * Append the given value to the current log by applying to the underlying value.
   */
  def :+->>(e: Option[Either[SqlException, A]] => LOG): RowAccess[A] =
    mapRowValue(_ :+->> e)

  /**
   * Prepend the given value to the current log.
   */
  def <-++:(e: LOG): RowAccess[A] =
    mapRowValue(e <-++: _)

  /**
   * Prepend the given value to the current log by applying to the underlying value.
   */
  def <<-+:(e: Option[Either[SqlException, A]] => LOG): RowAccess[A] =
    mapRowValue(e <<-+: _)

  /**
   * Set the log to be empty.
   */
  def resetLog: RowAccess[A] =
    mapRowValue(_.resetLog)
}

object RowAccess extends RowAccesss

trait RowAccesss {
  def rowAccess[A](f: Row => RowValue[A]): RowAccess[A] = new RowAccess[A] {
    val access = f
  }

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time, NClob}
  import Key._

  val arrayIndex: Int => RowAccess[java.sql.Array] = (columnIndex: Int) => rowAccess(_.arrayIndex(columnIndex))
  val arrayLabel: String => RowAccess[java.sql.Array] = (columnLabel: String) => rowAccess(_.arrayLabel(columnLabel))

  def asciiStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = (columnIndex: Int) => rowAccess(_.asciiStreamIndex(withInputStream)(columnIndex))
  def asciiStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = (columnLabel: String) => rowAccess(_.asciiStreamLabel(withInputStream)(columnLabel))

  val bigDecimalIndex: Int => RowAccess[java.math.BigDecimal] = (columnIndex: Int) => rowAccess(_.bigDecimalIndex(columnIndex))
  val bigDecimalLabel: String => RowAccess[java.math.BigDecimal] = (columnLabel: String) => rowAccess(_.bigDecimalLabel(columnLabel))

  def binaryStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = (columnIndex: Int) => rowAccess(_.binaryStreamIndex(withInputStream)(columnIndex))
  def binaryStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = (columnLabel: String) => rowAccess(_.binaryStreamLabel(withInputStream)(columnLabel))

  val blobIndex: Int => RowAccess[Blob] = (columnIndex: Int) => rowAccess(_.blobIndex(columnIndex))
  val blobLabel: String => RowAccess[Blob] = (columnLabel: String) => rowAccess(_.blobLabel(columnLabel))

  val booleanIndex: Int => RowAccess[Boolean] = (columnIndex: Int) => rowAccess(_.booleanIndex(columnIndex))
  val booleanLabel: String => RowAccess[Boolean] = (columnLabel: String) => rowAccess(_.booleanLabel(columnLabel))

  val byteIndex: Int => RowAccess[Byte] = (columnIndex: Int) => rowAccess(_.byteIndex(columnIndex))
  val byteLabel: String => RowAccess[Byte] = (columnLabel: String) => rowAccess(_.byteLabel(columnLabel))

  val bytesIndex: Int => RowAccess[Array[Byte]] = (columnIndex: Int) => rowAccess(_.bytesIndex(columnIndex))
  val bytesLabel: String => RowAccess[Array[Byte]] = (columnLabel: String) => rowAccess(_.bytesLabel(columnLabel))

  def characterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = (columnIndex: Int) => rowAccess(_.characterStreamIndex(withReader)(columnIndex))
  def characterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = (columnLabel: String) => rowAccess(_.characterStreamLabel(withReader)(columnLabel))

  val clobIndex: Int => RowAccess[Clob] = (columnIndex: Int) => rowAccess(_.clobIndex(columnIndex))
  val clobLabel: String => RowAccess[Clob] = (columnLabel: String) => rowAccess(_.clobLabel(columnLabel))

  val dateIndex: Int => RowAccess[Date] = (columnIndex: Int) => rowAccess(_.dateIndex(columnIndex))
  val dateLabel: String => RowAccess[Date] = (columnLabel: String) => rowAccess(_.dateLabel(columnLabel))
  def dateIndexCal(cal: Row.Cal): Int => RowAccess[Date] = (columnIndex: Int) => rowAccess(_.dateIndexCal(cal)(columnIndex))
  def dateLabelCal(cal: Row.Cal): String => RowAccess[Date] = (columnLabel: String) => rowAccess(_.dateLabelCal(cal)(columnLabel))

  val doubleIndex: Int => RowAccess[Double] = (columnIndex: Int) => rowAccess(_.doubleIndex(columnIndex))
  val doubleLabel: String => RowAccess[Double] = (columnLabel: String) => rowAccess(_.doubleLabel(columnLabel))

  val floatIndex: Int => RowAccess[Float] = (columnIndex: Int) => rowAccess(_.floatIndex(columnIndex))
  val floatLabel: String => RowAccess[Float] = (columnLabel: String) => rowAccess(_.floatLabel(columnLabel))

  val intIndex: Int => RowAccess[Int] = (columnIndex: Int) => rowAccess(_.intIndex(columnIndex))
  val intLabel: String => RowAccess[Int] = (columnLabel: String) => rowAccess(_.intLabel(columnLabel))

  val longIndex: Int => RowAccess[Long] = (columnIndex: Int) => rowAccess(_.longIndex(columnIndex))
  val longLabel: String => RowAccess[Long] = (columnLabel: String) => rowAccess(_.longLabel(columnLabel))

  def ncharacterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = (columnIndex: Int) => rowAccess(_.ncharacterStreamIndex(withReader)(columnIndex))
  def ncharacterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = (columnLabel: String) => rowAccess(_.ncharacterStreamLabel(withReader)(columnLabel))

  val nclobIndex: Int => RowAccess[NClob] = (columnIndex: Int) => rowAccess(_.nclobIndex(columnIndex))
  val nclobLabel: String => RowAccess[NClob] = (columnLabel: String) => rowAccess(_.nclobLabel(columnLabel))

  val nstringIndex: Int => RowAccess[String] = (columnIndex: Int) => rowAccess(_.nstringIndex(columnIndex))
  val nstringLabel: String => RowAccess[String] = (columnLabel: String) => rowAccess(_.nstringLabel(columnLabel))

  val objectIndex: Int => RowAccess[AnyRef] = (columnIndex: Int) => rowAccess(_.objectIndex(columnIndex))
  val objectLabel: String => RowAccess[AnyRef] = (columnLabel: String) => rowAccess(_.objectLabel(columnLabel))
  def objectMapIndex(m: Row.ObjectTypeMap): Int => RowAccess[AnyRef] = (columnIndex: Int) => rowAccess(_.objectMapIndex(m)(columnIndex))
  def objectMapLabel(m: Row.ObjectTypeMap): String => RowAccess[AnyRef] = (columnLabel: String) => rowAccess(_.objectMapLabel(m)(columnLabel))

  val refIndex: Int => RowAccess[Ref] = (columnIndex: Int) => rowAccess(_.refIndex(columnIndex))
  val refLabel: String => RowAccess[Ref] = (columnLabel: String) => rowAccess(_.refLabel(columnLabel))

  val rowIdIndex: Int => RowAccess[RowId] = (columnIndex: Int) => rowAccess(_.rowIdIndex(columnIndex))
  val rowIdLabel: String => RowAccess[RowId] = (columnLabel: String) => rowAccess(_.rowIdLabel(columnLabel))

  val shortIndex: Int => RowAccess[Short] = (columnIndex: Int) => rowAccess(_.shortIndex(columnIndex))
  val shortLabel: String => RowAccess[Short] = (columnLabel: String) => rowAccess(_.shortLabel(columnLabel))

  val sqlxmlIndex: Int => RowAccess[SQLXML] = (columnIndex: Int) => rowAccess(_.sqlxmlIndex(columnIndex))
  val sqlxmlLabel: String => RowAccess[SQLXML] = (columnLabel: String) => rowAccess(_.sqlxmlLabel(columnLabel))

  val stringIndex: Int => RowAccess[String] = (columnIndex: Int) => rowAccess(_.stringIndex(columnIndex))
  val stringLabel: String => RowAccess[String] = (columnLabel: String) => rowAccess(_.stringLabel(columnLabel))

  val timeIndex: Int => RowAccess[Time] = (columnIndex: Int) => rowAccess(_.timeIndex(columnIndex))
  val timeLabel: String => RowAccess[Time] = (columnLabel: String) => rowAccess(_.timeLabel(columnLabel))
  def timeIndexCal(cal: Row.Cal): Int => RowAccess[Time] = (columnIndex: Int) => rowAccess(_.timeIndexCal(cal)(columnIndex))
  def timeLabelCal(cal: Row.Cal): String => RowAccess[Time] = (columnLabel: String) => rowAccess(_.timeLabelCal(cal)(columnLabel))

  val timestampIndex: Int => RowAccess[Timestamp] = (columnIndex: Int) => rowAccess(_.timestampIndex(columnIndex))
  val timestampLabel: String => RowAccess[Timestamp] = (columnLabel: String) => rowAccess(_.timestampLabel(columnLabel))
  def timestampIndexCal(cal: Row.Cal): Int => RowAccess[Timestamp] = (columnIndex: Int) => rowAccess(_.timestampIndexCal(cal)(columnIndex))
  def timestampLabelCal(cal: Row.Cal): String => RowAccess[Timestamp] = (columnLabel: String) => rowAccess(_.timestampLabelCal(cal)(columnLabel))

  val urlIndex: Int => RowAccess[URL] = (columnIndex: Int) => rowAccess(_.urlIndex(columnIndex))
  val urlLabel: String => RowAccess[URL] = (columnLabel: String) => rowAccess(_.urlLabel(columnLabel))

  val keyIndex: Int => RowAccess[Key] = (columnIndex: Int) => longIndex(columnIndex) map (key(_))
  val keyLabel: String => RowAccess[Key] = (columnLabel: String) => longLabel(columnLabel) map (key(_))

  val possibleKeyIndex: Int => SqlAccess[Key] = (columnIndex: Int) => longIndex(columnIndex).possiblyNull map (_.toKey)
  val possibleKeyLabel: String => SqlAccess[Key] = (columnLabel: String) => longLabel(columnLabel).possiblyNull map (_.toKey)

  implicit val RowAccessFunctor: Functor[RowAccess] = new Functor[RowAccess] {
    def fmap[A, B](k: RowAccess[A], f: A => B) =
      k map f
  }

  implicit val RowAccessPure: Pure[RowAccess] = new Pure[RowAccess] {
    def pure[A](a: => A) =
      rowAccess(_ => a.η[RowValue])
  }

  implicit val RowAccessBind: Bind[RowAccess] = new Bind[RowAccess] {
    def bind[A, B](a: RowAccess[A], f: A => RowAccess[B]) =
      rowAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }
}
