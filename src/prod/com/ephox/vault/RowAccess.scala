package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowAccess[L, A] {
  val access: Row => RowValue[L, A]
  import SqlValue._
  import SqlAccess._
  import RowValue._
  import RowConnect._
  import RowQueryConnect._
  import PreparedStatementW._

  def -|>[T](iter: IterV[A, T]): RowQueryConnect[L, IterV[A, T]] =
    rowQueryConnect(query => rowConnect(c => try {
      val st = c prepareStatement query.sql
      st.set(query.bindings:_*)
      try {
        val r = st.executeQuery
        try {
          Row.resultSetRow(r).iterate[L, A, T](this)(iter)
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

  def -||>[T](iter: IterV[A, T]): RowQueryConnect[L, T] =
    -|>(iter) map (_.run)

  def map[B](f: A => B): RowAccess[L, B] = new RowAccess[L, B] {
    val access = (r: Row) => RowAccess.this.access(r) map f
  }

  def flatMap[B](f: A => RowAccess[L, B]): RowAccess[L, B] = new RowAccess[L, B] {
    val access = (r: Row) => RowAccess.this.access(r) flatMap (a => f(a).access(r))
  }

  def mapValue[B](f: RowValue[L, A] => RowValue[L, B]): RowAccess[L, B] = new RowAccess[L, B] {
    val access = (r: Row) => f(RowAccess.this.access(r))
  }

  def unifyNullWithMessage(message: String): SqlAccess[L, A] =
    sqlAccess((r: Row) => RowAccess.this.access(r) unifyNullWithMessage message)

  def unifyNull: SqlAccess[L, A] =
    sqlAccess((r: Row) => RowAccess.this.access(r).unifyNull)

  def possiblyNull: SqlAccess[L, PossiblyNull[A]] =
    sqlAccess(access(_).possiblyNull)

  def possiblyNullOr(d: => A): SqlAccess[L, A] =
    possiblyNull map (_ | d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

object RowAccess extends RowAccesss

trait RowAccesss {
  def rowAccess[L, A](f: Row => RowValue[L, A]): RowAccess[L, A] = new RowAccess[L, A] {
    val access = f
  }

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time}
  import Key._

  def arrayIndex[L](columnIndex: Int): RowAccess[L, java.sql.Array] = rowAccess(_.arrayIndex(columnIndex))
  def arrayLabel[L](columnLabel: String): RowAccess[L, java.sql.Array] = rowAccess(_.arrayLabel(columnLabel))

  def asciiStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[L, A] = rowAccess(_.asciiStreamIndex(columnIndex, withInputStream))
  def asciiStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowAccess[L, A] = rowAccess(_.asciiStreamLabel(columnLabel, withInputStream))

  def bigDecimalIndex[L](columnIndex: Int): RowAccess[L, java.math.BigDecimal] = rowAccess(_.bigDecimalIndex(columnIndex))
  def bigDecimalLabel[L](columnLabel: String): RowAccess[L, java.math.BigDecimal] = rowAccess(_.bigDecimalLabel(columnLabel))

  def binaryStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[L, A] = rowAccess(_.binaryStreamIndex(columnIndex, withInputStream))
  def binaryStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowAccess[L, A] = rowAccess(_.binaryStreamLabel(columnLabel, withInputStream))

  def blobIndex[L](columnIndex: Int): RowAccess[L, Blob] = rowAccess(_.blobIndex(columnIndex))
  def blobLabel[L](columnLabel: String): RowAccess[L, Blob] = rowAccess(_.blobLabel(columnLabel))

  def booleanIndex[L](columnIndex: Int): RowAccess[L, Boolean] = rowAccess(_.booleanIndex(columnIndex))
  def booleanLabel[L](columnLabel: String): RowAccess[L, Boolean] = rowAccess(_.booleanLabel(columnLabel))

  def byteIndex[L](columnIndex: Int): RowAccess[L, Byte] = rowAccess(_.byteIndex(columnIndex))
  def byteLabel[L](columnLabel: String): RowAccess[L, Byte] = rowAccess(_.byteLabel(columnLabel))

  def bytesIndex[L](columnIndex: Int): RowAccess[L, Array[Byte]] = rowAccess(_.bytesIndex(columnIndex))
  def bytesLabel[L](columnLabel: String): RowAccess[L, Array[Byte]] = rowAccess(_.bytesLabel(columnLabel))

  def characterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowAccess[L, A] = rowAccess(_.characterStreamIndex(columnIndex, withReader))
  def characterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowAccess[L, A] = rowAccess(_.characterStreamLabel(columnLabel, withReader))

  def clobIndex[L](columnIndex: Int): RowAccess[L, Clob] = rowAccess(_.clobIndex(columnIndex))
  def clobLabel[L](columnLabel: String): RowAccess[L, Clob] = rowAccess(_.clobLabel(columnLabel))

  def dateIndex[L](columnIndex: Int): RowAccess[L, Date] = rowAccess(_.dateIndex(columnIndex))
  def dateLabel[L](columnLabel: String): RowAccess[L, Date] = rowAccess(_.dateLabel(columnLabel))
  def dateIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccess[L, Date] = rowAccess(_.dateIndexCal(columnIndex, cal))
  def dateLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccess[L, Date] = rowAccess(_.dateLabelCal(columnLabel, cal))

  def doubleIndex[L](columnIndex: Int): RowAccess[L, Double] = rowAccess(_.doubleIndex(columnIndex))
  def doubleLabel[L](columnLabel: String): RowAccess[L, Double] = rowAccess(_.doubleLabel(columnLabel))

  def floatIndex[L](columnIndex: Int): RowAccess[L, Float] = rowAccess(_.floatIndex(columnIndex))
  def floatLabel[L](columnLabel: String): RowAccess[L, Float] = rowAccess(_.floatLabel(columnLabel))

  def intIndex[L](columnIndex: Int): RowAccess[L, Int] = rowAccess(_.intIndex(columnIndex))
  def intLabel[L](columnLabel: String): RowAccess[L, Int] = rowAccess(_.intLabel(columnLabel))

  def longIndex[L](columnIndex: Int): RowAccess[L, Long] = rowAccess(_.longIndex(columnIndex))
  def longLabel[L](columnLabel: String): RowAccess[L, Long] = rowAccess(_.longLabel(columnLabel))

  def ncharacterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowAccess[L, A] = rowAccess(_.ncharacterStreamIndex(columnIndex, withReader))
  def ncharacterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowAccess[L, A] = rowAccess(_.ncharacterStreamLabel(columnLabel, withReader))

  def nclobIndex[L](columnIndex: Int): RowAccess[L, Clob] = rowAccess(_.nclobIndex(columnIndex))
  def nclobLabel[L](columnLabel: String): RowAccess[L, Clob] = rowAccess(_.nclobLabel(columnLabel))

  def nstringIndex[L](columnIndex: Int): RowAccess[L, String] = rowAccess(_.nstringIndex(columnIndex))
  def nstringLabel[L](columnLabel: String): RowAccess[L, String] = rowAccess(_.nstringLabel(columnLabel))

  def objectIndex[L](columnIndex: Int): RowAccess[L, AnyRef] = rowAccess(_.objectIndex(columnIndex))
  def objectLabel[L](columnLabel: String): RowAccess[L, AnyRef] = rowAccess(_.objectLabel(columnLabel))
  def objectMapIndex[L](columnIndex: Int, m: Row.ObjectTypeMap): RowAccess[L, AnyRef] = rowAccess(_.objectMapIndex(columnIndex, m))
  def objectMapLabel[L](columnLabel: String, m: Row.ObjectTypeMap): RowAccess[L, AnyRef] = rowAccess(_.objectMapLabel(columnLabel, m))

  def refIndex[L](columnIndex: Int): RowAccess[L, Ref] = rowAccess(_.refIndex(columnIndex))
  def refLabel[L](columnLabel: String): RowAccess[L, Ref] = rowAccess(_.refLabel(columnLabel))

  def rowIdIndex[L](columnIndex: Int): RowAccess[L, RowId] = rowAccess(_.rowIdIndex(columnIndex))
  def rowIdLabel[L](columnLabel: String): RowAccess[L, RowId] = rowAccess(_.rowIdLabel(columnLabel))

  def shortIndex[L](columnIndex: Int): RowAccess[L, Short] = rowAccess(_.shortIndex(columnIndex))
  def shortLabel[L](columnLabel: String): RowAccess[L, Short] = rowAccess(_.shortLabel(columnLabel))

  def sqlxmlIndex[L](columnIndex: Int): RowAccess[L, SQLXML] = rowAccess(_.sqlxmlIndex(columnIndex))
  def sqlxmlLabel[L](columnLabel: String): RowAccess[L, SQLXML] = rowAccess(_.sqlxmlLabel(columnLabel))

  def stringIndex[L](columnIndex: Int): RowAccess[L, String] = rowAccess(_.stringIndex(columnIndex))
  def stringLabel[L](columnLabel: String): RowAccess[L, String] = rowAccess(_.stringLabel(columnLabel))

  def timeIndex[L](columnIndex: Int): RowAccess[L, Time] = rowAccess(_.timeIndex(columnIndex))
  def timeLabel[L](columnLabel: String): RowAccess[L, Time] = rowAccess(_.timeLabel(columnLabel))
  def timeIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccess[L, Time] = rowAccess(_.timeIndexCal(columnIndex, cal))
  def timeLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccess[L, Time] = rowAccess(_.timeLabelCal(columnLabel, cal))

  def timestampIndex[L](columnIndex: Int): RowAccess[L, Timestamp] = rowAccess(_.timestampIndex(columnIndex))
  def timestampLabel[L](columnLabel: String): RowAccess[L, Timestamp] = rowAccess(_.timestampLabel(columnLabel))
  def timestampIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccess[L, Timestamp] = rowAccess(_.timestampIndexCal(columnIndex, cal))
  def timestampLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccess[L, Timestamp] = rowAccess(_.timestampLabelCal(columnLabel, cal))

  def urlIndex[L](columnIndex: Int): RowAccess[L, URL] = rowAccess(_.urlIndex(columnIndex))
  def urlLabel[L](columnLabel: String): RowAccess[L, URL] = rowAccess(_.urlLabel(columnLabel))

  def idLabel[L](label: String): RowAccess[L, Key] = longLabel(label) map (key(_))
  def idIndex[L](index: Int): RowAccess[L, Key] = longIndex(index) map (key(_))

  def possibleIdLabel[L](label: String): SqlAccess[L, Key] = longLabel(label).possiblyNull map (_.toKey)

  def possibleIdIndex[L](index: Int): SqlAccess[L, Key] = longIndex(index).possiblyNull map (_.toKey)

  implicit def RowAccessFunctor[L]: Functor[({type λ[α]= RowAccess[L, α]})#λ] = new Functor[({type λ[α]= RowAccess[L, α]})#λ] {
    def fmap[A, B](k: RowAccess[L, A], f: A => B) =
      k map f
  }

  implicit def RowAccessPure[L, M[_]]: Pure[({type λ[α]= RowAccess[L, α]})#λ] = new Pure[({type λ[α]= RowAccess[L, α]})#λ] {
    def pure[A](a: => A) =
      rowAccess(_ => a.η[({type λ[α]= RowValue[L, α]})#λ])
  }

  implicit def RowAccessApply[L, M[_]]: Apply[({type λ[α]= RowAccess[L, α]})#λ] = new Apply[({type λ[α]= RowAccess[L, α]})#λ] {
    def apply[A, B](f: RowAccess[L, A => B], a: RowAccess[L, A]) = {
      rowAccess(r => a.access(r) <*> f.access(r))
    }
  }

  implicit def RowAccessApplicative[L]: Applicative[({type λ[α]= RowAccess[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowAccess[L, α]})#λ]

  implicit def RowAccessBind[L, M[_]]: Bind[({type λ[α]= RowAccess[L, α]})#λ] = new Bind[({type λ[α]= RowAccess[L, α]})#λ] {
    def bind[A, B](a: RowAccess[L, A], f: A => RowAccess[L, B]) =
      rowAccess(r => a.access(r) >>= (a => f(a) access (r)))
  }

  implicit def RowAccessMonad[L]: Monad[({type λ[α]= RowAccess[L, α]})#λ] = Monad.monad[({type λ[α]= RowAccess[L, α]})#λ]
}
