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

  private def log[T, U](c: Class[_], z: String, k: Row => U => RowValue[T]): U => RowAccess[T] =
    (column: U) => rowAccess(r => k(r)(column)) :+-> ("column type [" + c + "] at " + z + " [" + column + "]")

  private def logIndex[T](c: Class[_], k: Row => Int => RowValue[T]): Int => RowAccess[T] =
    log(c, "index", k)

  private def logLabel[T](c: Class[_], k: Row => String => RowValue[T]): String => RowAccess[T] =
    log(c, "label", k)

  val arrayIndex: Int => RowAccess[java.sql.Array] = logIndex(classOf[java.sql.Array], (_.arrayIndex))
  val arrayLabel: String => RowAccess[java.sql.Array] = logLabel(classOf[java.sql.Array], (_.arrayLabel))

  def asciiStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = logIndex(classOf[InputStream], (_.asciiStreamIndex(withInputStream)))
  def asciiStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = logLabel(classOf[InputStream], (_.asciiStreamLabel(withInputStream)))

  val bigDecimalIndex: Int => RowAccess[java.math.BigDecimal] = logIndex(classOf[java.math.BigDecimal], (_.bigDecimalIndex))
  val bigDecimalLabel: String => RowAccess[java.math.BigDecimal] = logLabel(classOf[java.sql.Array], (_.bigDecimalLabel))

  def binaryStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = logIndex(classOf[InputStream], (_.binaryStreamIndex(withInputStream)))
  def binaryStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = logLabel(classOf[InputStream], (_.binaryStreamLabel(withInputStream)))

  val blobIndex: Int => RowAccess[Blob] = logIndex(classOf[Blob], (_.blobIndex))
  val blobLabel: String => RowAccess[Blob] = logLabel(classOf[Blob], (_.blobLabel))

  val booleanIndex: Int => RowAccess[Boolean] = logIndex(classOf[Boolean], (_.booleanIndex))
  val booleanLabel: String => RowAccess[Boolean] = logLabel(classOf[Boolean], (_.booleanLabel))

  val byteIndex: Int => RowAccess[Byte] = logIndex(classOf[Byte], (_.byteIndex))
  val byteLabel: String => RowAccess[Byte] = logLabel(classOf[Byte], (_.byteLabel))

  val bytesIndex: Int => RowAccess[Array[Byte]] = logIndex(classOf[Array[Byte]], (_.bytesIndex))
  val bytesLabel: String => RowAccess[Array[Byte]] = logLabel(classOf[Array[Byte]], (_.bytesLabel))

  def characterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = logIndex(classOf[Reader], (_.characterStreamIndex(withReader)))
  def characterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = logLabel(classOf[Reader], (_.characterStreamLabel(withReader)))

  val clobIndex: Int => RowAccess[Clob] = logIndex(classOf[Clob], (_.clobIndex))
  val clobLabel: String => RowAccess[Clob] = logLabel(classOf[Clob], (_.clobLabel))

  val dateIndex: Int => RowAccess[Date] = logIndex(classOf[Date], (_.dateIndex))
  val dateLabel: String => RowAccess[Date] = logLabel(classOf[Date], (_.dateLabel))
  def dateIndexCal(cal: Row.Cal): Int => RowAccess[Date] = logIndex(classOf[Date], (_.dateIndexCal(cal)))
  def dateLabelCal(cal: Row.Cal): String => RowAccess[Date] = logLabel(classOf[Date], (_.dateLabelCal(cal)))

  val doubleIndex: Int => RowAccess[Double] = logIndex(classOf[Double], (_.doubleIndex))
  val doubleLabel: String => RowAccess[Double] = logLabel(classOf[Double], (_.doubleLabel))

  val floatIndex: Int => RowAccess[Float] = logIndex(classOf[Float], (_.floatIndex))
  val floatLabel: String => RowAccess[Float] = logLabel(classOf[Float], (_.floatLabel))

  val intIndex: Int => RowAccess[Int] = logIndex(classOf[Int], (_.intIndex))
  val intLabel: String => RowAccess[Int] = logLabel(classOf[Int], (_.intLabel))

  val longIndex: Int => RowAccess[Long] = logIndex(classOf[Long], (_.longIndex))
  val longLabel: String => RowAccess[Long] = logLabel(classOf[Long], (_.longLabel))

  def ncharacterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = logIndex(classOf[Reader], (_.ncharacterStreamIndex(withReader)))
  def ncharacterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = logLabel(classOf[Reader], (_.ncharacterStreamLabel(withReader)))

  val nclobIndex: Int => RowAccess[NClob] = logIndex(classOf[NClob], (_.nclobIndex))
  val nclobLabel: String => RowAccess[NClob] = logLabel(classOf[NClob], (_.nclobLabel))

  val nstringIndex: Int => RowAccess[String] = logIndex(classOf[String], (_.nstringIndex))
  val nstringLabel: String => RowAccess[String] = logLabel(classOf[String], (_.nstringLabel))

  val objectIndex: Int => RowAccess[AnyRef] = logIndex(classOf[AnyRef], (_.objectIndex))
  val objectLabel: String => RowAccess[AnyRef] = logLabel(classOf[AnyRef], (_.objectLabel))
  def objectMapIndex(m: Row.ObjectTypeMap): Int => RowAccess[AnyRef] = logIndex(classOf[Row.ObjectTypeMap], (_.objectMapIndex(m)))
  def objectMapLabel(m: Row.ObjectTypeMap): String => RowAccess[AnyRef] = logLabel(classOf[Row.ObjectTypeMap], (_.objectMapLabel(m)))

  val refIndex: Int => RowAccess[Ref] = logIndex(classOf[Ref], (_.refIndex))
  val refLabel: String => RowAccess[Ref] = logLabel(classOf[Ref], (_.refLabel))

  val rowIdIndex: Int => RowAccess[RowId] = logIndex(classOf[RowId], (_.rowIdIndex))
  val rowIdLabel: String => RowAccess[RowId] = logLabel(classOf[RowId], (_.rowIdLabel))

  val shortIndex: Int => RowAccess[Short] = logIndex(classOf[Short], (_.shortIndex))
  val shortLabel: String => RowAccess[Short] = logLabel(classOf[Short], (_.shortLabel))

  val sqlxmlIndex: Int => RowAccess[SQLXML] = logIndex(classOf[SQLXML], (_.sqlxmlIndex))
  val sqlxmlLabel: String => RowAccess[SQLXML] = logLabel(classOf[SQLXML], (_.sqlxmlLabel))

  val stringIndex: Int => RowAccess[String] = logIndex(classOf[String], (_.stringIndex))
  val stringLabel: String => RowAccess[String] = logLabel(classOf[String], (_.stringLabel))

  val timeIndex: Int => RowAccess[Time] = logIndex(classOf[Time], (_.timeIndex))
  val timeLabel: String => RowAccess[Time] = logLabel(classOf[Time], (_.timeLabel))
  def timeIndexCal(cal: Row.Cal): Int => RowAccess[Time] = logIndex(classOf[Time], (_.timeIndexCal(cal)))
  def timeLabelCal(cal: Row.Cal): String => RowAccess[Time] = logLabel(classOf[Time], (_.timeLabelCal(cal)))

  val timestampIndex: Int => RowAccess[Timestamp] = logIndex(classOf[Timestamp], (_.timestampIndex))
  val timestampLabel: String => RowAccess[Timestamp] = logLabel(classOf[Timestamp], (_.timestampLabel))
  def timestampIndexCal(cal: Row.Cal): Int => RowAccess[Timestamp] = logIndex(classOf[Timestamp], (_.timestampIndexCal(cal)))
  def timestampLabelCal(cal: Row.Cal): String => RowAccess[Timestamp] = logLabel(classOf[Timestamp], (_.timestampLabelCal(cal)))

  val urlIndex: Int => RowAccess[URL] = logIndex(classOf[URL], (_.urlIndex))
  val urlLabel: String => RowAccess[URL] = logLabel(classOf[URL], (_.urlLabel))

  val keyIndex: Int => RowAccess[Key] = logIndex(classOf[Key], (r => columnIndex => r.longIndex(columnIndex) map (key(_))))
  val keyLabel: String => RowAccess[Key] = logLabel(classOf[Key], (r => columnLabel => r.longLabel(columnLabel) map (key(_))))

  val possibleKeyIndex: Int => SqlAccess[Key] =
    i => logIndex(classOf[Key], r => columnIndex => r.longIndex(columnIndex))(i).possiblyNull map (_.toKey)
  val possibleKeyLabel: String => SqlAccess[Key] =
    i => logLabel(classOf[Key], r => columnLabel => r.longLabel(columnLabel))(i).possiblyNull map (_.toKey)

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
