package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowAccessor[A] {
  val access: Row => RowValue[A]

  def -|>[T](iter: IterV[A, T]): SqlRowAccess[IterV[A, T]] =
    sqlRowAccess(query => rowConnector(c => try {
      query.fold(
        (sql, bindings) => {
          val st = c prepareStatement sql
          st.set(bindings:_*)
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
        })
    } catch {
      case e: SqlException => rowError(e)
      case x                        => throw x
    }))

  def -||>[T](iter: IterV[A, T]): SqlRowAccess[T] =
    -|>(iter) map (_.run)

  def map[B](f: A => B): RowAccessor[B] = new RowAccessor[B] {
    val access = (r: Row) => RowAccessor.this.access(r) map f
  }

  def flatMap[B](f: A => RowAccessor[B]): RowAccessor[B] = new RowAccessor[B] {
    val access = (r: Row) => RowAccessor.this.access(r) flatMap (a => f(a).access(r))
  }

  def possiblyNull: RowAccessor[Option[A]] =
    rowAccessor(access(_).possiblyNull)

  def possiblyNullOr(d: => A): RowAccessor[A] =
    possiblyNull ∘ (_ getOrElse d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

trait RowAccessors {
  def rowAccessor[A](f: Row => RowValue[A]): RowAccessor[A] = new RowAccessor[A] {
    val access = f
  }

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time}

  def arrayIndex(columnIndex: Int): RowAccessor[java.sql.Array] = rowAccessor(_.arrayIndex(columnIndex))
  def arrayLabel(columnLabel: String): RowAccessor[java.sql.Array] = rowAccessor(_.arrayLabel(columnLabel))

  def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.asciiStreamIndex(columnIndex, withInputStream))
  def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.asciiStreamLabel(columnLabel, withInputStream))

  def bigDecimalIndex(columnIndex: Int): RowAccessor[java.math.BigDecimal] = rowAccessor(_.bigDecimalIndex(columnIndex))
  def bigDecimalLabel(columnLabel: String): RowAccessor[java.math.BigDecimal] = rowAccessor(_.bigDecimalLabel(columnLabel))

  def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.binaryStreamIndex(columnIndex, withInputStream))
  def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.binaryStreamLabel(columnLabel, withInputStream))

  def blobIndex(columnIndex: Int): RowAccessor[Blob] = rowAccessor(_.blobIndex(columnIndex))
  def blobLabel(columnLabel: String): RowAccessor[Blob] = rowAccessor(_.blobLabel(columnLabel))

  def booleanIndex(columnIndex: Int): RowAccessor[Boolean] = rowAccessor(_.booleanIndex(columnIndex))
  def booleanLabel(columnLabel: String): RowAccessor[Boolean] = rowAccessor(_.booleanLabel(columnLabel))

  def byteIndex(columnIndex: Int): RowAccessor[Byte] = rowAccessor(_.byteIndex(columnIndex))
  def byteLabel(columnLabel: String): RowAccessor[Byte] = rowAccessor(_.byteLabel(columnLabel))

  def bytesIndex(columnIndex: Int): RowAccessor[Array[Byte]] = rowAccessor(_.bytesIndex(columnIndex))
  def bytesLabel(columnLabel: String): RowAccessor[Array[Byte]] = rowAccessor(_.bytesLabel(columnLabel))

  def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.characterStreamIndex(columnIndex, withReader))
  def characterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.characterStreamLabel(columnLabel, withReader))

  def clobIndex(columnIndex: Int): RowAccessor[Clob] = rowAccessor(_.clobIndex(columnIndex))
  def clobLabel(columnLabel: String): RowAccessor[Clob] = rowAccessor(_.clobLabel(columnLabel))

  def dateIndex(columnIndex: Int): RowAccessor[Date] = rowAccessor(_.dateIndex(columnIndex))
  def dateLabel(columnLabel: String): RowAccessor[Date] = rowAccessor(_.dateLabel(columnLabel))
  def dateIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Date] = rowAccessor(_.dateIndexCal(columnIndex, cal))
  def dateLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Date] = rowAccessor(_.dateLabelCal(columnLabel, cal))

  def doubleIndex(columnIndex: Int): RowAccessor[Double] = rowAccessor(_.doubleIndex(columnIndex))
  def doubleLabel(columnLabel: String): RowAccessor[Double] = rowAccessor(_.doubleLabel(columnLabel))

  def floatIndex(columnIndex: Int): RowAccessor[Float] = rowAccessor(_.floatIndex(columnIndex))
  def floatLabel(columnLabel: String): RowAccessor[Float] = rowAccessor(_.floatLabel(columnLabel))

  def intIndex(columnIndex: Int): RowAccessor[Int] = rowAccessor(_.intIndex(columnIndex))
  def intLabel(columnLabel: String): RowAccessor[Int] = rowAccessor(_.intLabel(columnLabel))

  def longIndex(columnIndex: Int): RowAccessor[Long] = rowAccessor(_.longIndex(columnIndex))
  def longLabel(columnLabel: String): RowAccessor[Long] = rowAccessor(_.longLabel(columnLabel))

  def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.ncharacterStreamIndex(columnIndex, withReader))
  def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.ncharacterStreamLabel(columnLabel, withReader))

  def nclobIndex(columnIndex: Int): RowAccessor[Clob] = rowAccessor(_.nclobIndex(columnIndex))
  def nclobLabel(columnLabel: String): RowAccessor[Clob] = rowAccessor(_.nclobLabel(columnLabel))

  def nstringIndex(columnIndex: Int): RowAccessor[String] = rowAccessor(_.nstringIndex(columnIndex))
  def nstringLabel(columnLabel: String): RowAccessor[String] = rowAccessor(_.nstringLabel(columnLabel))

  def objectIndex(columnIndex: Int): RowAccessor[AnyRef] = rowAccessor(_.objectIndex(columnIndex))
  def objectLabel(columnLabel: String): RowAccessor[AnyRef] = rowAccessor(_.objectLabel(columnLabel))
  def objectMapIndex(columnIndex: Int, m: Row.ObjectTypeMap): RowAccessor[AnyRef] = rowAccessor(_.objectMapIndex(columnIndex, m))
  def objectMapLabel(columnLabel: String, m: Row.ObjectTypeMap): RowAccessor[AnyRef] = rowAccessor(_.objectMapLabel(columnLabel, m))

  def refIndex(columnIndex: Int): RowAccessor[Ref] = rowAccessor(_.refIndex(columnIndex))
  def refLabel(columnLabel: String): RowAccessor[Ref] = rowAccessor(_.refLabel(columnLabel))

  def rowIdIndex(columnIndex: Int): RowAccessor[RowId] = rowAccessor(_.rowIdIndex(columnIndex))
  def rowIdLabel(columnLabel: String): RowAccessor[RowId] = rowAccessor(_.rowIdLabel(columnLabel))

  def shortIndex(columnIndex: Int): RowAccessor[Short] = rowAccessor(_.shortIndex(columnIndex))
  def shortLabel(columnLabel: String): RowAccessor[Short] = rowAccessor(_.shortLabel(columnLabel))

  def sqlxmlIndex(columnIndex: Int): RowAccessor[SQLXML] = rowAccessor(_.sqlxmlIndex(columnIndex))
  def sqlxmlLabel(columnLabel: String): RowAccessor[SQLXML] = rowAccessor(_.sqlxmlLabel(columnLabel))

  def stringIndex(columnIndex: Int): RowAccessor[String] = rowAccessor(_.stringIndex(columnIndex))
  def stringLabel(columnLabel: String): RowAccessor[String] = rowAccessor(_.stringLabel(columnLabel))

  def timeIndex(columnIndex: Int): RowAccessor[Time] = rowAccessor(_.timeIndex(columnIndex))
  def timeLabel(columnLabel: String): RowAccessor[Time] = rowAccessor(_.timeLabel(columnLabel))
  def timeIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Time] = rowAccessor(_.timeIndexCal(columnIndex, cal))
  def timeLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Time] = rowAccessor(_.timeLabelCal(columnLabel, cal))

  def timestampIndex(columnIndex: Int): RowAccessor[Timestamp] = rowAccessor(_.timestampIndex(columnIndex))
  def timestampLabel(columnLabel: String): RowAccessor[Timestamp] = rowAccessor(_.timestampLabel(columnLabel))
  def timestampIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Timestamp] = rowAccessor(_.timestampIndexCal(columnIndex, cal))
  def timestampLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Timestamp] = rowAccessor(_.timestampLabelCal(columnLabel, cal))

  def urlIndex(columnIndex: Int): RowAccessor[URL] = rowAccessor(_.urlIndex(columnIndex))
  def urlLabel(columnLabel: String): RowAccessor[URL] = rowAccessor(_.urlLabel(columnLabel))

  def idLabel(label: String) = longLabel(label) map (Key.key(_))
  def idIndex(index: Int) = longIndex(index) map (Key.key(_))

  def possibleIdLabel(label: String) = longLabel(label).possiblyNull map ({
    case None => Key.nokey
    case Some(x) => Key.key(x)
  })

  def possibleIdIndex(index: Int) = longIndex(index).possiblyNull map ({
    case None => Key.nokey
    case Some(x) => Key.key(x)
  })

  implicit def RowAccessorFunctor: Functor[RowAccessor] = new Functor[RowAccessor] {
    def fmap[A, B](k: RowAccessor[A], f: A => B) =
      rowAccessor(r => k.access(r) map f)
  }

  implicit def RowAccessorPure[M[_]]: Pure[RowAccessor] = new Pure[RowAccessor] {
    def pure[A](a: => A) =
      rowAccessor(_ => a.η[RowValue])
  }

  implicit def RowAccessorApply[M[_]]: Apply[RowAccessor] = new Apply[RowAccessor] {
    def apply[A, B](f: RowAccessor[A => B], a: RowAccessor[A]) = {
      rowAccessor(r => a.access(r) <*> f.access(r))
    }
  }

  implicit def RowAccessorBind[M[_]]: Bind[RowAccessor] = new Bind[RowAccessor] {
    def bind[A, B](a: RowAccessor[A], f: A => RowAccessor[B]) =
      rowAccessor(r => a.access(r) >>= (a => f(a) access (r)))
  }
}
