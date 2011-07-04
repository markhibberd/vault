package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import RowValue._
import java.sql.ResultSet
import com.ephox.vault.SqlExceptionContext._

sealed trait RowAccess[A] {
  import RowAccess._
  import SqlAccess._
  import RowValue._
  import RowConnect._
  import RowQueryConnect._
  import PreparedStatementW._

  def access: Row => RowValue[A] =
    this match {
      case RowAccess_(v) => v
    }

  def -|>[T](iter: IterV[A, T]): RowQueryConnect[IterV[A, T]] =
    rowQueryConnect(query => rowConnect(c => {
      tryRowValue(c prepareStatement query.sql).mapError(_ setQuery query) flatMap (st =>
        st.set(query.bindings:_*).toRowValue >>=| (
          try {
            tryRowValue(st.executeQuery) flatMap ((r: ResultSet) =>
              try {
                Row.resultSetRow(r).iterate[A, T](this)(iter).mapError(e => e.setQueryPreparedStatement(query, st))
              } finally {
                r.close
              })
          } finally {
            st.close
          }).mapError(e => e.setQuery(query)))
    }))

  def -||>[T](iter: IterV[A, T]): RowQueryConnect[T] =
    -|>(iter) map (_.run)

  def map[B](f: A => B): RowAccess[B] = rowAccess((r: Row) => RowAccess.this.access(r) map f)

  def flatMap[B](f: A => RowAccess[B]): RowAccess[B] = rowAccess((r: Row) => RowAccess.this.access(r) flatMap (a => f(a).access(r)))

  def mapRowValue[B](f: RowValue[A] => RowValue[B]): RowAccess[B] = rowAccess((r: Row) => f(RowAccess.this.access(r)))

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

  def asList: RowAccess[List[A]] =
    toList.toRowAccess

  def asOption: RowAccess[Option[A]] =
    possiblyNull.toRowAccess map (_.toOption)

  // alias for possiblyNullOr
  def |?(d: => A) = possiblyNullOr(d)

  /**
   * Lifts this value into a possibly null value. The following holds:
   *
   * forall r s. r.liftPossiblyNull.access(s).isNotNull
   */
  def liftPossiblyNull: RowAccess[PossiblyNull[A]] =
    possiblyNull.toRowAccess
}
private final case class RowAccess_[A](v: Row => RowValue[A]) extends RowAccess[A]

object RowAccess extends RowAccesss

trait RowAccesss {
  def rowAccess[A](f: Row => RowValue[A]): RowAccess[A] = RowAccess_(f)

  def rowAccessNiceNull[A](f: Row => RowValue[A], note: String): RowAccess[A] =
    rowAccess(r => {
      val x = f(r)
      x.fold(
        _ => x,
        _ => x,
        _ => rowNullMsg(note)
      )
    })

  import java.io.{Reader, InputStream}
  import java.net.URL
  import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time, NClob}
  import CampanionKey._

  private def forEither[T, U](c: Class[_], z: String, k: Row => U => RowValue[T]): U => RowAccess[T] =
    (column: U) => rowAccessNiceNull(r => k(r)(column), "column type [" + c.getName + "] at " + z + " [" + column + "]")

  private def forIndex[T](c: Class[_], k: Row => Int => RowValue[T]): Int => RowAccess[T] =
    forEither(c, "index", k)

  private def forLabel[T](c: Class[_], k: Row => String => RowValue[T]): String => RowAccess[T] =
    forEither(c, "label", k)

  private def forLookup[T](c: Class[_], k: Row => Int => RowValue[T]): (String, String) => RowAccess[T] =
    (table: String, column: String) =>
      rowAccess(r =>
        r.indexFor(table, column) match {
          case None => rowError(sqlExceptionContext(new SqlException("No column [" + table + "." + column + "] in result [" + r.columns + "].")))
          case Some(index) => {
            val x = k(r)(index)
            x.fold(
              _ => x,
              _ => x,
              _ => rowNullMsg("column type [" + c.getName + "] at lookup [" +  table + "." + column + "@" + index + "]")
            )
          }
        }
      )


  val arrayIndex: Int => RowAccess[java.sql.Array] = forIndex(classOf[java.sql.Array], (_.arrayIndex))
  val arrayLabel: String => RowAccess[java.sql.Array] = forLabel(classOf[java.sql.Array], (_.arrayLabel))
  val arrayLookup: (String, String) => RowAccess[java.sql.Array] = forLookup(classOf[java.sql.Array], (_.arrayIndex))

  def asciiStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = forIndex(classOf[InputStream], (_.asciiStreamIndex(withInputStream)))
  def asciiStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = forLabel(classOf[InputStream], (_.asciiStreamLabel(withInputStream)))
  def asciiStreamLookup[A](withInputStream: InputStream => A): (String, String) => RowAccess[A] = forLookup(classOf[InputStream], (_.asciiStreamIndex(withInputStream)))

  val bigDecimalIndex: Int => RowAccess[java.math.BigDecimal] = forIndex(classOf[java.math.BigDecimal], (_.bigDecimalIndex))
  val bigDecimalLabel: String => RowAccess[java.math.BigDecimal] = forLabel(classOf[java.sql.Array], (_.bigDecimalLabel))
  val bigDecimalLookup: (String, String) => RowAccess[java.math.BigDecimal] = forLookup(classOf[java.math.BigDecimal], (_.bigDecimalIndex))

  def binaryStreamIndex[A](withInputStream: InputStream => A): Int => RowAccess[A] = forIndex(classOf[InputStream], (_.binaryStreamIndex(withInputStream)))
  def binaryStreamLabel[A](withInputStream: InputStream => A): String => RowAccess[A] = forLabel(classOf[InputStream], (_.binaryStreamLabel(withInputStream)))
  def binaryStreamLookup[A](withInputStream: InputStream => A): (String, String) => RowAccess[A] = forLookup(classOf[InputStream], (_.binaryStreamIndex(withInputStream)))

  val blobIndex: Int => RowAccess[Blob] = forIndex(classOf[Blob], (_.blobIndex))
  val blobLabel: String => RowAccess[Blob] = forLabel(classOf[Blob], (_.blobLabel))
  val blobLookup: (String, String) => RowAccess[Blob] = forLookup(classOf[Blob], (_.blobIndex))

  val booleanIndex: Int => RowAccess[Boolean] = forIndex(classOf[Boolean], (_.booleanIndex))
  val booleanLabel: String => RowAccess[Boolean] = forLabel(classOf[Boolean], (_.booleanLabel))
  val booleanLookup: (String, String) => RowAccess[Boolean] = forLookup(classOf[Boolean], (_.booleanIndex))

  val byteIndex: Int => RowAccess[Byte] = forIndex(classOf[Byte], (_.byteIndex))
  val byteLabel: String => RowAccess[Byte] = forLabel(classOf[Byte], (_.byteLabel))
  val byteLookup: (String, String) => RowAccess[Byte] = forLookup(classOf[Byte], (_.byteIndex))

  val bytesIndex: Int => RowAccess[Array[Byte]] = forIndex(classOf[Array[Byte]], (_.bytesIndex))
  val bytesLabel: String => RowAccess[Array[Byte]] = forLabel(classOf[Array[Byte]], (_.bytesLabel))
  val bytesLookup: (String, String) => RowAccess[Array[Byte]] = forLookup(classOf[Array[Byte]], (_.bytesIndex))

  def characterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = forIndex(classOf[Reader], (_.characterStreamIndex(withReader)))
  def characterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = forLabel(classOf[Reader], (_.characterStreamLabel(withReader)))
  def characterStreamLookup[A](withReader: Reader => A): (String, String) => RowAccess[A] = forLookup(classOf[Reader], (_.characterStreamIndex(withReader)))

  val clobIndex: Int => RowAccess[Clob] = forIndex(classOf[Clob], (_.clobIndex))
  val clobLabel: String => RowAccess[Clob] = forLabel(classOf[Clob], (_.clobLabel))
  val clobLookup: (String, String) => RowAccess[Clob] = forLookup(classOf[Clob], (_.clobIndex))

  val dateIndex: Int => RowAccess[Date] = forIndex(classOf[Date], (_.dateIndex))
  val dateLabel: String => RowAccess[Date] = forLabel(classOf[Date], (_.dateLabel))
  val dateLookup: (String, String) => RowAccess[Date] = forLookup(classOf[Date], (_.dateIndex))

  def dateIndexCal(cal: Row.Cal): Int => RowAccess[Date] = forIndex(classOf[Date], (_.dateIndexCal(cal)))
  def dateLabelCal(cal: Row.Cal): String => RowAccess[Date] = forLabel(classOf[Date], (_.dateLabelCal(cal)))
  def dateLookupCal(cal: Row.Cal): (String, String) => RowAccess[Date] = forLookup(classOf[Date], (_.dateIndexCal(cal)))

  val doubleIndex: Int => RowAccess[Double] = forIndex(classOf[Double], (_.doubleIndex))
  val doubleLabel: String => RowAccess[Double] = forLabel(classOf[Double], (_.doubleLabel))
  val doubleLookup: (String, String) => RowAccess[Double] = forLookup(classOf[Double], (_.doubleIndex))

  val floatIndex: Int => RowAccess[Float] = forIndex(classOf[Float], (_.floatIndex))
  val floatLabel: String => RowAccess[Float] = forLabel(classOf[Float], (_.floatLabel))
  val floatLookup: (String, String) => RowAccess[Float] = forLookup(classOf[Float], (_.floatIndex))

  val intIndex: Int => RowAccess[Int] = forIndex(classOf[Int], (_.intIndex))
  val intLabel: String => RowAccess[Int] = forLabel(classOf[Int], (_.intLabel))
  val intLookup: (String, String) => RowAccess[Int] = forLookup(classOf[Int], (_.intIndex))

  val longIndex: Int => RowAccess[Long] = forIndex(classOf[Long], (_.longIndex))
  val longLabel: String => RowAccess[Long] = forLabel(classOf[Long], (_.longLabel))
  val longLookup: (String, String) => RowAccess[Long] = forLookup(classOf[Long], (_.longIndex))

  val objectIndex: Int => RowAccess[AnyRef] = forIndex(classOf[AnyRef], (_.objectIndex))
  val objectLabel: String => RowAccess[AnyRef] = forLabel(classOf[AnyRef], (_.objectLabel))
  val objectLookup: (String, String) => RowAccess[AnyRef] = forLookup(classOf[AnyRef], (_.objectIndex))

  def objectMapIndex(m: Row.ObjectTypeMap): Int => RowAccess[AnyRef] = forIndex(classOf[Row.ObjectTypeMap], (_.objectMapIndex(m)))
  def objectMapLabel(m: Row.ObjectTypeMap): String => RowAccess[AnyRef] = forLabel(classOf[Row.ObjectTypeMap], (_.objectMapLabel(m)))
  def objectMapLookup(m: Row.ObjectTypeMap): (String, String) => RowAccess[AnyRef] = forLookup(classOf[Row.ObjectTypeMap], (_.objectMapIndex(m)))

  val refIndex: Int => RowAccess[Ref] = forIndex(classOf[Ref], (_.refIndex))
  val refLabel: String => RowAccess[Ref] = forLabel(classOf[Ref], (_.refLabel))
  val refLookup: (String, String) => RowAccess[Ref] = forLookup(classOf[Ref], (_.refIndex))

  val shortIndex: Int => RowAccess[Short] = forIndex(classOf[Short], (_.shortIndex))
  val shortLabel: String => RowAccess[Short] = forLabel(classOf[Short], (_.shortLabel))
  val shortLookup: (String, String) => RowAccess[Short] = forLookup(classOf[Short], (_.shortIndex))

  val stringIndex: Int => RowAccess[String] = forIndex(classOf[String], (_.stringIndex))
  val stringLabel: String => RowAccess[String] = forLabel(classOf[String], (_.stringLabel))
  val stringLookup: (String, String) => RowAccess[String] = forLookup(classOf[String], (_.stringIndex))

  val timeIndex: Int => RowAccess[Time] = forIndex(classOf[Time], (_.timeIndex))
  val timeLabel: String => RowAccess[Time] = forLabel(classOf[Time], (_.timeLabel))
  val timeLookup: (String, String) => RowAccess[Time] = forLookup(classOf[Time], (_.timeIndex))

  def timeIndexCal(cal: Row.Cal): Int => RowAccess[Time] = forIndex(classOf[Time], (_.timeIndexCal(cal)))
  def timeLabelCal(cal: Row.Cal): String => RowAccess[Time] = forLabel(classOf[Time], (_.timeLabelCal(cal)))
  def timeLookupCal(cal: Row.Cal): (String, String) => RowAccess[Time] = forLookup(classOf[Time], (_.timeIndexCal(cal)))

  val timestampIndex: Int => RowAccess[Timestamp] = forIndex(classOf[Timestamp], (_.timestampIndex))
  val timestampLabel: String => RowAccess[Timestamp] = forLabel(classOf[Timestamp], (_.timestampLabel))
  val timestampLookup: (String, String) => RowAccess[Timestamp] = forLookup(classOf[Timestamp], (_.timestampIndex))

  def timestampIndexCal(cal: Row.Cal): Int => RowAccess[Timestamp] = forIndex(classOf[Timestamp], (_.timestampIndexCal(cal)))
  def timestampLabelCal(cal: Row.Cal): String => RowAccess[Timestamp] = forLabel(classOf[Timestamp], (_.timestampLabelCal(cal)))
  def timestampLookupCal(cal: Row.Cal): (String, String) => RowAccess[Timestamp] = forLookup(classOf[Timestamp], (_.timestampIndexCal(cal)))

  val urlIndex: Int => RowAccess[URL] = forIndex(classOf[URL], (_.urlIndex))
  val urlLabel: String => RowAccess[URL] = forLabel(classOf[URL], (_.urlLabel))
  val urlLookup: (String, String) => RowAccess[URL] = forLookup(classOf[URL], (_.urlIndex))

  val keyIndex: Int => RowAccess[Key] = forIndex(classOf[Key], (r => columnIndex => r.longIndex(columnIndex) map (key(_))))
  val keyLabel: String => RowAccess[Key] = forLabel(classOf[Key], (r => columnLabel => r.longLabel(columnLabel) map (key(_))))
  val keyLookup: (String, String) => RowAccess[Key] = forLookup(classOf[Key], (r => columnIndex => r.longIndex(columnIndex) map (key(_))))

  val possibleKeyIndex: Int => SqlAccess[Key] =
    i => forIndex(classOf[Key], r => columnIndex => r.longIndex(columnIndex))(i).possiblyNull map (_.toKey)
  val possibleKeyLabel: String => SqlAccess[Key] =
    i => forLabel(classOf[Key], r => columnLabel => r.longLabel(columnLabel))(i).possiblyNull map (_.toKey)
  val possibleKeyLookup: (String, String) => SqlAccess[Key] =
    (t, c) => forLookup(classOf[Key], r => columnIndex => r.longIndex(columnIndex))(t, c).possiblyNull map (_.toKey)



//   -- JDBC 4.0 disabled for the time being --
//
//  def ncharacterStreamIndex[A](withReader: Reader => A): Int => RowAccess[A] = forIndex(classOf[Reader], (_.ncharacterStreamIndex(withReader)))
//  def ncharacterStreamLabel[A](withReader: Reader => A): String => RowAccess[A] = forLabel(classOf[Reader], (_.ncharacterStreamLabel(withReader)))
//
//  val nclobIndex: Int => RowAccess[NClob] = forIndex(classOf[NClob], (_.nclobIndex))
//  val nclobLabel: String => RowAccess[NClob] = forLabel(classOf[NClob], (_.nclobLabel))
//
//  val nstringIndex: Int => RowAccess[String] = forIndex(classOf[String], (_.nstringIndex))
//  val nstringLabel: String => RowAccess[String] = forLabel(classOf[String], (_.nstringLabel))
//
//  val rowIdIndex: Int => RowAccess[RowId] = forIndex(classOf[RowId], (_.rowIdIndex))
//  val rowIdLabel: String => RowAccess[RowId] = forLabel(classOf[RowId], (_.rowIdLabel))
//
//  val sqlxmlIndex: Int => RowAccess[SQLXML] = forIndex(classOf[SQLXML], (_.sqlxmlIndex))
//  val sqlxmlLabel: String => RowAccess[SQLXML] = forLabel(classOf[SQLXML], (_.sqlxmlLabel))


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
