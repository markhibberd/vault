package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowAccessor[L, A] {
  val access: Row => RowValue[L, A]

  def -|>[T](iter: IterV[A, T]): SqlRowAccess[L, IterV[A, T]] =
    sqlRowAccess(query => rowConnect(c => try {
      query.fold(
        (sql, bindings) => {
          val st = c prepareStatement sql
          st.set(bindings:_*)
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
        })
    } catch {
      case e: SqlException => rowError(e)
      case x                        => throw x
    }))

  def -||>[T](iter: IterV[A, T]): SqlRowAccess[L, T] =
    -|>(iter) map (_.run)

  def map[B](f: A => B): RowAccessor[L, B] = new RowAccessor[L, B] {
    val access = (r: Row) => RowAccessor.this.access(r) map f
  }

  def flatMap[B](f: A => RowAccessor[L, B]): RowAccessor[L, B] = new RowAccessor[L, B] {
    val access = (r: Row) => RowAccessor.this.access(r) flatMap (a => f(a).access(r))
  }

  def possiblyNull: RowAccessor[L, Option[A]] =
    // rowAccessor(access(_).possiblyNull)
    error("todo")

  def possiblyNullOr(d: => A): RowAccessor[L, A] =
    possiblyNull map (_ getOrElse d)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)
}

trait RowAccessors {
  def rowAccessor[L, A](f: Row => RowValue[L, A]): RowAccessor[L, A] = new RowAccessor[L, A] {
    val access = f
  }

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time}

  def arrayIndex[L](columnIndex: Int): RowAccessor[L, java.sql.Array] = rowAccessor(_.arrayIndex(columnIndex))
  def arrayLabel[L](columnLabel: String): RowAccessor[L, java.sql.Array] = rowAccessor(_.arrayLabel(columnLabel))

  def asciiStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[L, A] = rowAccessor(_.asciiStreamIndex(columnIndex, withInputStream))
  def asciiStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[L, A] = rowAccessor(_.asciiStreamLabel(columnLabel, withInputStream))

  def bigDecimalIndex[L](columnIndex: Int): RowAccessor[L, java.math.BigDecimal] = rowAccessor(_.bigDecimalIndex(columnIndex))
  def bigDecimalLabel[L](columnLabel: String): RowAccessor[L, java.math.BigDecimal] = rowAccessor(_.bigDecimalLabel(columnLabel))

  def binaryStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[L, A] = rowAccessor(_.binaryStreamIndex(columnIndex, withInputStream))
  def binaryStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[L, A] = rowAccessor(_.binaryStreamLabel(columnLabel, withInputStream))

  def blobIndex[L](columnIndex: Int): RowAccessor[L, Blob] = rowAccessor(_.blobIndex(columnIndex))
  def blobLabel[L](columnLabel: String): RowAccessor[L, Blob] = rowAccessor(_.blobLabel(columnLabel))

  def booleanIndex[L](columnIndex: Int): RowAccessor[L, Boolean] = rowAccessor(_.booleanIndex(columnIndex))
  def booleanLabel[L](columnLabel: String): RowAccessor[L, Boolean] = rowAccessor(_.booleanLabel(columnLabel))

  def byteIndex[L](columnIndex: Int): RowAccessor[L, Byte] = rowAccessor(_.byteIndex(columnIndex))
  def byteLabel[L](columnLabel: String): RowAccessor[L, Byte] = rowAccessor(_.byteLabel(columnLabel))

  def bytesIndex[L](columnIndex: Int): RowAccessor[L, Array[Byte]] = rowAccessor(_.bytesIndex(columnIndex))
  def bytesLabel[L](columnLabel: String): RowAccessor[L, Array[Byte]] = rowAccessor(_.bytesLabel(columnLabel))

  def characterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowAccessor[L, A] = rowAccessor(_.characterStreamIndex(columnIndex, withReader))
  def characterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowAccessor[L, A] = rowAccessor(_.characterStreamLabel(columnLabel, withReader))

  def clobIndex[L](columnIndex: Int): RowAccessor[L, Clob] = rowAccessor(_.clobIndex(columnIndex))
  def clobLabel[L](columnLabel: String): RowAccessor[L, Clob] = rowAccessor(_.clobLabel(columnLabel))

  def dateIndex[L](columnIndex: Int): RowAccessor[L, Date] = rowAccessor(_.dateIndex(columnIndex))
  def dateLabel[L](columnLabel: String): RowAccessor[L, Date] = rowAccessor(_.dateLabel(columnLabel))
  def dateIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccessor[L, Date] = rowAccessor(_.dateIndexCal(columnIndex, cal))
  def dateLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccessor[L, Date] = rowAccessor(_.dateLabelCal(columnLabel, cal))

  def doubleIndex[L](columnIndex: Int): RowAccessor[L, Double] = rowAccessor(_.doubleIndex(columnIndex))
  def doubleLabel[L](columnLabel: String): RowAccessor[L, Double] = rowAccessor(_.doubleLabel(columnLabel))

  def floatIndex[L](columnIndex: Int): RowAccessor[L, Float] = rowAccessor(_.floatIndex(columnIndex))
  def floatLabel[L](columnLabel: String): RowAccessor[L, Float] = rowAccessor(_.floatLabel(columnLabel))

  def intIndex[L](columnIndex: Int): RowAccessor[L, Int] = rowAccessor(_.intIndex(columnIndex))
  def intLabel[L](columnLabel: String): RowAccessor[L, Int] = rowAccessor(_.intLabel(columnLabel))

  def longIndex[L](columnIndex: Int): RowAccessor[L, Long] = rowAccessor(_.longIndex(columnIndex))
  def longLabel[L](columnLabel: String): RowAccessor[L, Long] = rowAccessor(_.longLabel(columnLabel))

  def ncharacterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowAccessor[L, A] = rowAccessor(_.ncharacterStreamIndex(columnIndex, withReader))
  def ncharacterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowAccessor[L, A] = rowAccessor(_.ncharacterStreamLabel(columnLabel, withReader))

  def nclobIndex[L](columnIndex: Int): RowAccessor[L, Clob] = rowAccessor(_.nclobIndex(columnIndex))
  def nclobLabel[L](columnLabel: String): RowAccessor[L, Clob] = rowAccessor(_.nclobLabel(columnLabel))

  def nstringIndex[L](columnIndex: Int): RowAccessor[L, String] = rowAccessor(_.nstringIndex(columnIndex))
  def nstringLabel[L](columnLabel: String): RowAccessor[L, String] = rowAccessor(_.nstringLabel(columnLabel))

  def objectIndex[L](columnIndex: Int): RowAccessor[L, AnyRef] = rowAccessor(_.objectIndex(columnIndex))
  def objectLabel[L](columnLabel: String): RowAccessor[L, AnyRef] = rowAccessor(_.objectLabel(columnLabel))
  def objectMapIndex[L](columnIndex: Int, m: Row.ObjectTypeMap): RowAccessor[L, AnyRef] = rowAccessor(_.objectMapIndex(columnIndex, m))
  def objectMapLabel[L](columnLabel: String, m: Row.ObjectTypeMap): RowAccessor[L, AnyRef] = rowAccessor(_.objectMapLabel(columnLabel, m))

  def refIndex[L](columnIndex: Int): RowAccessor[L, Ref] = rowAccessor(_.refIndex(columnIndex))
  def refLabel[L](columnLabel: String): RowAccessor[L, Ref] = rowAccessor(_.refLabel(columnLabel))

  def rowIdIndex[L](columnIndex: Int): RowAccessor[L, RowId] = rowAccessor(_.rowIdIndex(columnIndex))
  def rowIdLabel[L](columnLabel: String): RowAccessor[L, RowId] = rowAccessor(_.rowIdLabel(columnLabel))

  def shortIndex[L](columnIndex: Int): RowAccessor[L, Short] = rowAccessor(_.shortIndex(columnIndex))
  def shortLabel[L](columnLabel: String): RowAccessor[L, Short] = rowAccessor(_.shortLabel(columnLabel))

  def sqlxmlIndex[L](columnIndex: Int): RowAccessor[L, SQLXML] = rowAccessor(_.sqlxmlIndex(columnIndex))
  def sqlxmlLabel[L](columnLabel: String): RowAccessor[L, SQLXML] = rowAccessor(_.sqlxmlLabel(columnLabel))

  def stringIndex[L](columnIndex: Int): RowAccessor[L, String] = rowAccessor(_.stringIndex(columnIndex))
  def stringLabel[L](columnLabel: String): RowAccessor[L, String] = rowAccessor(_.stringLabel(columnLabel))

  def timeIndex[L](columnIndex: Int): RowAccessor[L, Time] = rowAccessor(_.timeIndex(columnIndex))
  def timeLabel[L](columnLabel: String): RowAccessor[L, Time] = rowAccessor(_.timeLabel(columnLabel))
  def timeIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccessor[L, Time] = rowAccessor(_.timeIndexCal(columnIndex, cal))
  def timeLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccessor[L, Time] = rowAccessor(_.timeLabelCal(columnLabel, cal))

  def timestampIndex[L](columnIndex: Int): RowAccessor[L, Timestamp] = rowAccessor(_.timestampIndex(columnIndex))
  def timestampLabel[L](columnLabel: String): RowAccessor[L, Timestamp] = rowAccessor(_.timestampLabel(columnLabel))
  def timestampIndexCal[L](columnIndex: Int, cal: Row.Cal): RowAccessor[L, Timestamp] = rowAccessor(_.timestampIndexCal(columnIndex, cal))
  def timestampLabelCal[L](columnLabel: String, cal: Row.Cal): RowAccessor[L, Timestamp] = rowAccessor(_.timestampLabelCal(columnLabel, cal))

  def urlIndex[L](columnIndex: Int): RowAccessor[L, URL] = rowAccessor(_.urlIndex(columnIndex))
  def urlLabel[L](columnLabel: String): RowAccessor[L, URL] = rowAccessor(_.urlLabel(columnLabel))

  def idLabel[L](label: String): RowAccessor[L, Key] = longLabel(label) map (Key.key(_))
  def idIndex[L](index: Int): RowAccessor[L, Key] = longIndex(index) map (Key.key(_))

  def possibleIdLabel[L](label: String): RowAccessor[L, Key] = longLabel(label).possiblyNull map ({
    case None => Key.nokey
    case Some(x) => Key.key(x)
  })

  def possibleIdIndex[L](index: Int): RowAccessor[L, Key] = longIndex(index).possiblyNull map ({
    case None => Key.nokey
    case Some(x) => Key.key(x)
  })

  implicit def RowAccessorFunctor[L]: Functor[({type λ[α]= RowAccessor[L, α]})#λ] = new Functor[({type λ[α]= RowAccessor[L, α]})#λ] {
    def fmap[A, B](k: RowAccessor[L, A], f: A => B) =
      k map f
  }

  implicit def RowAccessorPure[L, M[_]]: Pure[({type λ[α]= RowAccessor[L, α]})#λ] = new Pure[({type λ[α]= RowAccessor[L, α]})#λ] {
    def pure[A](a: => A) =
      rowAccessor(_ => a.η[({type λ[α]= RowValue[L, α]})#λ])
  }

  implicit def RowAccessorApply[L, M[_]]: Apply[({type λ[α]= RowAccessor[L, α]})#λ] = new Apply[({type λ[α]= RowAccessor[L, α]})#λ] {
    def apply[A, B](f: RowAccessor[L, A => B], a: RowAccessor[L, A]) = {
      rowAccessor(r => a.access(r) <*> f.access(r))
    }
  }

  implicit def RowAccessorApplicative[L]: Applicative[({type λ[α]= RowAccessor[L, α]})#λ] = Applicative.applicative[({type λ[α]= RowAccessor[L, α]})#λ]

  implicit def RowAccessorBind[L, M[_]]: Bind[({type λ[α]= RowAccessor[L, α]})#λ] = new Bind[({type λ[α]= RowAccessor[L, α]})#λ] {
    def bind[A, B](a: RowAccessor[L, A], f: A => RowAccessor[L, B]) =
      rowAccessor(r => a.access(r) >>= (a => f(a) access (r)))
  }

  implicit def RowAccessorMonad[L]: Monad[({type λ[α]= RowAccessor[L, α]})#λ] = Monad.monad[({type λ[α]= RowAccessor[L, α]})#λ]
}
