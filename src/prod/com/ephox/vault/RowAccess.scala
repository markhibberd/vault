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

  def mapValue[B](f: RowValue[A] => RowValue[B]): RowAccess[B] = new RowAccess[B] {
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
}

object RowAccess extends RowAccesss

trait RowAccesss {
  def rowAccess[A](f: Row => RowValue[A]): RowAccess[A] = new RowAccess[A] {
    val access = f
  }

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time}
  import Key._

  def arrayIndex(columnIndex: Int): RowAccess[java.sql.Array] = rowAccess(_.arrayIndex(columnIndex))
  def arrayLabel(columnLabel: String): RowAccess[java.sql.Array] = rowAccess(_.arrayLabel(columnLabel))

  def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[A] = rowAccess(_.asciiStreamIndex(columnIndex, withInputStream))
  def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccess[A] = rowAccess(_.asciiStreamLabel(columnLabel, withInputStream))

  def bigDecimalIndex(columnIndex: Int): RowAccess[java.math.BigDecimal] = rowAccess(_.bigDecimalIndex(columnIndex))
  def bigDecimalLabel(columnLabel: String): RowAccess[java.math.BigDecimal] = rowAccess(_.bigDecimalLabel(columnLabel))

  def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[A] = rowAccess(_.binaryStreamIndex(columnIndex, withInputStream))
  def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccess[A] = rowAccess(_.binaryStreamLabel(columnLabel, withInputStream))

  def blobIndex(columnIndex: Int): RowAccess[Blob] = rowAccess(_.blobIndex(columnIndex))
  def blobLabel(columnLabel: String): RowAccess[Blob] = rowAccess(_.blobLabel(columnLabel))

  def booleanIndex(columnIndex: Int): RowAccess[Boolean] = rowAccess(_.booleanIndex(columnIndex))
  def booleanLabel(columnLabel: String): RowAccess[Boolean] = rowAccess(_.booleanLabel(columnLabel))

  def byteIndex(columnIndex: Int): RowAccess[Byte] = rowAccess(_.byteIndex(columnIndex))
  def byteLabel(columnLabel: String): RowAccess[Byte] = rowAccess(_.byteLabel(columnLabel))

  def bytesIndex(columnIndex: Int): RowAccess[Array[Byte]] = rowAccess(_.bytesIndex(columnIndex))
  def bytesLabel(columnLabel: String): RowAccess[Array[Byte]] = rowAccess(_.bytesLabel(columnLabel))

  def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccess[A] = rowAccess(_.characterStreamIndex(columnIndex, withReader))
  def characterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccess[A] = rowAccess(_.characterStreamLabel(columnLabel, withReader))

  def clobIndex(columnIndex: Int): RowAccess[Clob] = rowAccess(_.clobIndex(columnIndex))
  def clobLabel(columnLabel: String): RowAccess[Clob] = rowAccess(_.clobLabel(columnLabel))

  def dateIndex(columnIndex: Int): RowAccess[Date] = rowAccess(_.dateIndex(columnIndex))
  def dateLabel(columnLabel: String): RowAccess[Date] = rowAccess(_.dateLabel(columnLabel))
  def dateIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Date] = rowAccess(_.dateIndexCal(columnIndex, cal))
  def dateLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Date] = rowAccess(_.dateLabelCal(columnLabel, cal))

  def doubleIndex(columnIndex: Int): RowAccess[Double] = rowAccess(_.doubleIndex(columnIndex))
  def doubleLabel(columnLabel: String): RowAccess[Double] = rowAccess(_.doubleLabel(columnLabel))

  def floatIndex(columnIndex: Int): RowAccess[Float] = rowAccess(_.floatIndex(columnIndex))
  def floatLabel(columnLabel: String): RowAccess[Float] = rowAccess(_.floatLabel(columnLabel))

  def intIndex(columnIndex: Int): RowAccess[Int] = rowAccess(_.intIndex(columnIndex))
  def intLabel(columnLabel: String): RowAccess[Int] = rowAccess(_.intLabel(columnLabel))

  def longIndex(columnIndex: Int): RowAccess[Long] = rowAccess(_.longIndex(columnIndex))
  def longLabel(columnLabel: String): RowAccess[Long] = rowAccess(_.longLabel(columnLabel))

  def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccess[A] = rowAccess(_.ncharacterStreamIndex(columnIndex, withReader))
  def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccess[A] = rowAccess(_.ncharacterStreamLabel(columnLabel, withReader))

  def nclobIndex(columnIndex: Int): RowAccess[Clob] = rowAccess(_.nclobIndex(columnIndex))
  def nclobLabel(columnLabel: String): RowAccess[Clob] = rowAccess(_.nclobLabel(columnLabel))

  def nstringIndex(columnIndex: Int): RowAccess[String] = rowAccess(_.nstringIndex(columnIndex))
  def nstringLabel(columnLabel: String): RowAccess[String] = rowAccess(_.nstringLabel(columnLabel))

  def objectIndex(columnIndex: Int): RowAccess[AnyRef] = rowAccess(_.objectIndex(columnIndex))
  def objectLabel(columnLabel: String): RowAccess[AnyRef] = rowAccess(_.objectLabel(columnLabel))
  def objectMapIndex(columnIndex: Int, m: Row.ObjectTypeMap): RowAccess[AnyRef] = rowAccess(_.objectMapIndex(columnIndex, m))
  def objectMapLabel(columnLabel: String, m: Row.ObjectTypeMap): RowAccess[AnyRef] = rowAccess(_.objectMapLabel(columnLabel, m))

  def refIndex(columnIndex: Int): RowAccess[Ref] = rowAccess(_.refIndex(columnIndex))
  def refLabel(columnLabel: String): RowAccess[Ref] = rowAccess(_.refLabel(columnLabel))

  def rowIdIndex(columnIndex: Int): RowAccess[RowId] = rowAccess(_.rowIdIndex(columnIndex))
  def rowIdLabel(columnLabel: String): RowAccess[RowId] = rowAccess(_.rowIdLabel(columnLabel))

  def shortIndex(columnIndex: Int): RowAccess[Short] = rowAccess(_.shortIndex(columnIndex))
  def shortLabel(columnLabel: String): RowAccess[Short] = rowAccess(_.shortLabel(columnLabel))

  def sqlxmlIndex(columnIndex: Int): RowAccess[SQLXML] = rowAccess(_.sqlxmlIndex(columnIndex))
  def sqlxmlLabel(columnLabel: String): RowAccess[SQLXML] = rowAccess(_.sqlxmlLabel(columnLabel))

  def stringIndex(columnIndex: Int): RowAccess[String] = rowAccess(_.stringIndex(columnIndex))
  def stringLabel(columnLabel: String): RowAccess[String] = rowAccess(_.stringLabel(columnLabel))

  def timeIndex(columnIndex: Int): RowAccess[Time] = rowAccess(_.timeIndex(columnIndex))
  def timeLabel(columnLabel: String): RowAccess[Time] = rowAccess(_.timeLabel(columnLabel))
  def timeIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Time] = rowAccess(_.timeIndexCal(columnIndex, cal))
  def timeLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Time] = rowAccess(_.timeLabelCal(columnLabel, cal))

  def timestampIndex(columnIndex: Int): RowAccess[Timestamp] = rowAccess(_.timestampIndex(columnIndex))
  def timestampLabel(columnLabel: String): RowAccess[Timestamp] = rowAccess(_.timestampLabel(columnLabel))
  def timestampIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Timestamp] = rowAccess(_.timestampIndexCal(columnIndex, cal))
  def timestampLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Timestamp] = rowAccess(_.timestampLabelCal(columnLabel, cal))

  def urlIndex(columnIndex: Int): RowAccess[URL] = rowAccess(_.urlIndex(columnIndex))
  def urlLabel(columnLabel: String): RowAccess[URL] = rowAccess(_.urlLabel(columnLabel))

  def keyLabel(label: String): RowAccess[Key] = longLabel(label) map (key(_))
  def keyIndex(index: Int): RowAccess[Key] = longIndex(index) map (key(_))

  def possibleKeyLabel(label: String): SqlAccess[Key] = longLabel(label).possiblyNull map (_.toKey)

  def possibleKeyIndex(index: Int): SqlAccess[Key] = longIndex(index).possiblyNull map (_.toKey)

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
