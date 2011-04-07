package com.ephox.vault

import scalaz._, Scalaz._
import java.io.{Reader, InputStream}
import java.util.Calendar
import java.sql.{Timestamp, Time, SQLXML, RowId, Ref, Date, Clob, Blob, ResultSet, NClob}
import java.net.URL

sealed trait Row {
  def iterate[A, T](a: RowAccess[A]): IterV[A, T] => RowValue[IterV[A, T]]

  val arrayIndex: Int => RowValue[java.sql.Array]
  val arrayLabel: String => RowValue[java.sql.Array]

  def asciiStreamIndex[A](withInputStream: InputStream => A): Int => RowValue[A]
  def asciiStreamLabel[A](withInputStream: InputStream => A): String => RowValue[A]

  val bigDecimalIndex: Int => RowValue[java.math.BigDecimal]
  val bigDecimalLabel: String => RowValue[java.math.BigDecimal]

  def binaryStreamIndex[A](withInputStream: InputStream => A): Int => RowValue[A]
  def binaryStreamLabel[A](withInputStream: InputStream => A): String => RowValue[A]

  val blobIndex: Int => RowValue[Blob]
  val blobLabel: String => RowValue[Blob]

  val booleanIndex: Int => RowValue[Boolean]
  val booleanLabel: String => RowValue[Boolean]

  val byteIndex: Int => RowValue[Byte]
  val byteLabel: String => RowValue[Byte]

  val bytesIndex: Int => RowValue[Array[Byte]]
  val bytesLabel: String => RowValue[Array[Byte]]

  def characterStreamIndex[A](withReader: Reader => A): Int => RowValue[A]
  def characterStreamLabel[A](withReader: Reader => A): String => RowValue[A]

  val clobIndex: Int => RowValue[Clob]
  val clobLabel: String => RowValue[Clob]

  val dateIndex: Int => RowValue[Date]
  val dateLabel: String => RowValue[Date]
  def dateIndexCal(cal: Row.Cal): Int => RowValue[Date]
  def dateLabelCal(cal: Row.Cal): String => RowValue[Date]

  val doubleIndex: Int => RowValue[Double]
  val doubleLabel: String => RowValue[Double]

  val floatIndex: Int => RowValue[Float]
  val floatLabel: String => RowValue[Float]

  val intIndex: Int => RowValue[Int]
  val intLabel: String => RowValue[Int]

  val longIndex: Int => RowValue[Long]
  val longLabel: String => RowValue[Long]

  def ncharacterStreamIndex[A](withReader: Reader => A): Int => RowValue[A]
  def ncharacterStreamLabel[A](withReader: Reader => A): String => RowValue[A]

  val nclobIndex: Int => RowValue[NClob]
  val nclobLabel: String => RowValue[NClob]

  val nstringIndex: Int => RowValue[String]
  val nstringLabel: String => RowValue[String]

  val objectIndex: Int => RowValue[AnyRef]
  val objectLabel: String => RowValue[AnyRef]
  def objectMapIndex(m: Row.ObjectTypeMap): Int => RowValue[AnyRef]
  def objectMapLabel(m: Row.ObjectTypeMap): String => RowValue[AnyRef]

  val refIndex: Int => RowValue[Ref]
  val refLabel: String => RowValue[Ref]

  val rowIdIndex: Int => RowValue[RowId]
  val rowIdLabel: String => RowValue[RowId]

  val shortIndex: Int => RowValue[Short]
  val shortLabel: String => RowValue[Short]

  val sqlxmlIndex: Int => RowValue[SQLXML]
  val sqlxmlLabel: String => RowValue[SQLXML]

  val stringIndex: Int => RowValue[String]
  val stringLabel: String => RowValue[String]

  val timeIndex: Int => RowValue[Time]
  val timeLabel: String => RowValue[Time]
  def timeIndexCal(cal: Row.Cal): Int => RowValue[Time]
  def timeLabelCal(cal: Row.Cal): String => RowValue[Time]

  val timestampIndex: Int => RowValue[Timestamp]
  val timestampLabel: String => RowValue[Timestamp]
  def timestampIndexCal(cal: Row.Cal): Int => RowValue[Timestamp]
  def timestampLabelCal(cal: Row.Cal): String => RowValue[Timestamp]

  val urlIndex: Int => RowValue[URL]
  val urlLabel: String => RowValue[URL]

  val keyIndex: Int => RowValue[Key]
  val keyLabel: String => RowValue[Key]

  val possibleKeyIndex: Int => SqlValue[Key]
  val possibleKeyLabel: String => SqlValue[Key]
}

object Row {
  import RowValue._
  import Key._

  type ObjectTypeMap = java.util.Map[String, Class[_]]
  type Cal = Calendar

  private[vault] def resultSetRow(r: ResultSet): Row = new Row {
    private def tryResultSet[A](a: => A): RowValue[A] =
      // todo capture context
      try {
        // very dangerous, beware of effect on ResultSet (wasNull)
        val z = a
        if(r.wasNull) rowNull else z.η[RowValue]
      } catch {
        case e: SqlValue.SqlException => rowError(e)
        case x => throw x
      }

    def iterate[A, T](ra: RowAccess[A]) =
      iter => {
        def loop(i: IterV[A, T]): RowValue[IterV[A, T]] =
          i.fold((a, ip) => i.η[RowValue],
                 k => {
                   val hasMore = r.next
                   if (hasMore) ra.access(Row.resultSetRow(r)) flatMap (t => loop(k(IterV.El(t))))
                   else i.η[RowValue]
                 })
        loop(iter)
      }

    val arrayIndex = (columnIndex: Int) =>
      tryResultSet(r.getArray(columnIndex))
    val arrayLabel = (columnLabel: String) =>
      tryResultSet(r.getArray(columnLabel))

    def asciiStreamIndex[A](withInputStream: InputStream => A) = (columnIndex: Int) => {
      val s = r.getAsciiStream(columnIndex)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def asciiStreamLabel[A](withInputStream: InputStream => A) = (columnLabel: String) => {
      val s = r.getAsciiStream(columnLabel)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    val bigDecimalIndex = (columnIndex: Int) =>
      tryResultSet(r.getBigDecimal(columnIndex))
    val bigDecimalLabel = (columnLabel: String) =>
      tryResultSet(r.getBigDecimal(columnLabel))

    def binaryStreamIndex[A](withInputStream: InputStream => A) = (columnIndex: Int) => {
      val s = r.getBinaryStream(columnIndex)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def binaryStreamLabel[A](withInputStream: InputStream => A) = (columnLabel: String) => {
      val s = r.getBinaryStream(columnLabel)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    val blobIndex = (columnIndex: Int) =>
      tryResultSet(r.getBlob(columnIndex))
    val blobLabel = (columnLabel: String) =>
      tryResultSet(r.getBlob(columnLabel))

    val booleanIndex = (columnIndex: Int) =>
      tryResultSet(r.getBoolean(columnIndex))
    val booleanLabel = (columnLabel: String) =>
      tryResultSet(r.getBoolean(columnLabel))

    val byteIndex = (columnIndex: Int) =>
      tryResultSet(r.getByte(columnIndex))
    val byteLabel = (columnLabel: String) =>
      tryResultSet(r.getByte(columnLabel))

    val bytesIndex = (columnIndex: Int) =>
      tryResultSet(r.getBytes(columnIndex))
    val bytesLabel = (columnLabel: String) =>
      tryResultSet(r.getBytes(columnLabel))

    def characterStreamIndex[A](withReader: Reader => A) = (columnIndex: Int) => {
      val s = r.getCharacterStream(columnIndex)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def characterStreamLabel[A](withReader: Reader => A) = (columnLabel: String) => {
      val s = r.getCharacterStream(columnLabel)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    val clobIndex = (columnIndex: Int) =>
      tryResultSet(r.getClob(columnIndex))
    val clobLabel = (columnLabel: String) =>
      tryResultSet(r.getClob(columnLabel))

    val dateIndex = (columnIndex: Int) =>
      tryResultSet(r.getDate(columnIndex))
    val dateLabel = (columnLabel: String) =>
      tryResultSet(r.getDate(columnLabel))
    def dateIndexCal(cal: Cal) = (columnIndex: Int) =>
      tryResultSet(r.getDate(columnIndex, cal))
    def dateLabelCal(cal: Cal) = (columnLabel: String) =>
      tryResultSet(r.getDate(columnLabel, cal))

    val doubleIndex = (columnIndex: Int) =>
      tryResultSet(r.getDouble(columnIndex))
    val doubleLabel = (columnLabel: String) =>
      tryResultSet(r.getDouble(columnLabel))

    val floatIndex = (columnIndex: Int) =>
      tryResultSet(r.getFloat(columnIndex))
    val floatLabel = (columnLabel: String) =>
      tryResultSet(r.getFloat(columnLabel))

    val intIndex = (columnIndex: Int) =>
      tryResultSet(r.getInt(columnIndex))
    val intLabel = (columnLabel: String) =>
      tryResultSet(r.getInt(columnLabel))

    val longIndex = (columnIndex: Int) =>
      tryResultSet(r.getLong(columnIndex))
    val longLabel = (columnLabel: String) =>
      tryResultSet(r.getLong(columnLabel))

    def ncharacterStreamIndex[A](withReader: Reader => A) = (columnIndex: Int) => {
      val s = r.getNCharacterStream(columnIndex)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def ncharacterStreamLabel[A](withReader: Reader => A) = (columnLabel: String) => {
      val s = r.getNCharacterStream(columnLabel)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    val nclobIndex = (columnIndex: Int) =>
      tryResultSet(r.getNClob(columnIndex))
    val nclobLabel = (columnLabel: String) =>
      tryResultSet(r.getNClob(columnLabel))

    val nstringIndex = (columnIndex: Int) =>
      tryResultSet(r.getNString(columnIndex))
    val nstringLabel = (columnLabel: String) =>
      tryResultSet(r.getNString(columnLabel))

    val objectIndex = (columnIndex: Int) =>
      tryResultSet(r.getObject(columnIndex))
    val objectLabel = (columnLabel: String) =>
      tryResultSet(r.getObject(columnLabel))
    def objectMapIndex(m: ObjectTypeMap) = (columnIndex: Int) =>
      tryResultSet(r.getObject(columnIndex, m))
    def objectMapLabel(m: ObjectTypeMap) = (columnLabel: String) =>
      tryResultSet(r.getObject(columnLabel, m))

    val refIndex = (columnIndex: Int) =>
      tryResultSet(r.getRef(columnIndex))
    val refLabel = (columnLabel: String) =>
      tryResultSet(r.getRef(columnLabel))

    val rowIdIndex = (columnIndex: Int) =>
      tryResultSet(r.getRowId(columnIndex))
    val rowIdLabel = (columnLabel: String) =>
      tryResultSet(r.getRowId(columnLabel))

    val shortIndex = (columnIndex: Int) =>
      tryResultSet(r.getShort(columnIndex))
    val shortLabel = (columnLabel: String) =>
      tryResultSet(r.getShort(columnLabel))

    val sqlxmlIndex = (columnIndex: Int) =>
      tryResultSet(r.getSQLXML(columnIndex))
    val sqlxmlLabel = (columnLabel: String) =>
      tryResultSet(r.getSQLXML(columnLabel))

    val stringIndex = (columnIndex: Int) =>
      tryResultSet(r.getString(columnIndex))
    val stringLabel = (columnLabel: String) =>
      tryResultSet(r.getString(columnLabel))

    val timeIndex = (columnIndex: Int) =>
      tryResultSet(r.getTime(columnIndex))
    val timeLabel = (columnLabel: String) =>
      tryResultSet(r.getTime(columnLabel))
    def timeIndexCal(cal: Cal) = (columnIndex: Int) =>
      tryResultSet(r.getTime(columnIndex, cal))
    def timeLabelCal(cal: Cal) = (columnLabel: String) =>
      tryResultSet(r.getTime(columnLabel, cal))

    val timestampIndex = (columnIndex: Int) =>
      tryResultSet(r.getTimestamp(columnIndex))
    val timestampLabel = (columnLabel: String) =>
      tryResultSet(r.getTimestamp(columnLabel))
    def timestampIndexCal(cal: Cal) = (columnIndex: Int) =>
      tryResultSet(r.getTimestamp(columnIndex))
    def timestampLabelCal(cal: Cal) = (columnLabel: String) =>
      tryResultSet(r.getTimestamp(columnLabel))

    val urlIndex = (columnIndex: Int) =>
      tryResultSet(r.getURL(columnIndex))
    val urlLabel = (columnLabel: String) =>
      tryResultSet(r.getURL(columnLabel))

    val keyIndex = (columnIndex: Int) =>
      longIndex(columnIndex) map (key(_))
    val keyLabel = (columnLabel: String) =>
    longLabel(columnLabel) map (key(_))

    val possibleKeyIndex = (columnIndex: Int) =>
      longIndex(columnIndex).possiblyNull map (_.toKey)
    val possibleKeyLabel = (columnLabel: String) =>
    longLabel(columnLabel).possiblyNull map (_.toKey)
  }
}
