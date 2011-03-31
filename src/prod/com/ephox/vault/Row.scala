package com.ephox.vault

import scalaz._, Scalaz._
import java.io.{Reader, InputStream}
import java.util.Calendar
import java.sql.{Timestamp, Time, SQLXML, RowId, Ref, Date, Clob, Blob, ResultSet}
import java.net.URL

sealed trait Row {
  def iterate[A, T](a: RowAccess[A]): IterV[A, T] => RowValue[IterV[A, T]]

  def arrayIndex(columnIndex: Int): RowValue[java.sql.Array]
  def arrayLabel(columnLabel: String): RowValue[java.sql.Array]

  def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowValue[A]
  def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowValue[A]

  def bigDecimalIndex(columnIndex: Int): RowValue[java.math.BigDecimal]
  def bigDecimalLabel(columnLabel: String): RowValue[java.math.BigDecimal]

  def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowValue[A]
  def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowValue[A]

  def blobIndex(columnIndex: Int): RowValue[Blob]
  def blobLabel(columnLabel: String): RowValue[Blob]

  def booleanIndex(columnIndex: Int): RowValue[Boolean]
  def booleanLabel(columnLabel: String): RowValue[Boolean]

  def byteIndex(columnIndex: Int): RowValue[Byte]
  def byteLabel(columnLabel: String): RowValue[Byte]

  def bytesIndex(columnIndex: Int): RowValue[Array[Byte]]
  def bytesLabel(columnLabel: String): RowValue[Array[Byte]]

  def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowValue[A]
  def characterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowValue[A]

  def clobIndex(columnIndex: Int): RowValue[Clob]
  def clobLabel(columnLabel: String): RowValue[Clob]

  def dateIndex(columnIndex: Int): RowValue[Date]
  def dateLabel(columnLabel: String): RowValue[Date]
  def dateIndexCal(columnIndex: Int, cal: Row.Cal): RowValue[Date]
  def dateLabelCal(columnLabel: String, cal: Row.Cal): RowValue[Date]

  def doubleIndex(columnIndex: Int): RowValue[Double]
  def doubleLabel(columnLabel: String): RowValue[Double]

  def floatIndex(columnIndex: Int): RowValue[Float]
  def floatLabel(columnLabel: String): RowValue[Float]

  def intIndex(columnIndex: Int): RowValue[Int]
  def intLabel(columnLabel: String): RowValue[Int]

  def longIndex(columnIndex: Int): RowValue[Long]
  def longLabel(columnLabel: String): RowValue[Long]

  def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowValue[A]
  def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowValue[A]

  def nclobIndex(columnIndex: Int): RowValue[Clob]
  def nclobLabel(columnLabel: String): RowValue[Clob]

  def nstringIndex(columnIndex: Int): RowValue[String]
  def nstringLabel(columnLabel: String): RowValue[String]

  def objectIndex(columnIndex: Int): RowValue[AnyRef]
  def objectLabel(columnLabel: String): RowValue[AnyRef]
  def objectMapIndex(columnIndex: Int, m: Row.ObjectTypeMap): RowValue[AnyRef]
  def objectMapLabel(columnLabel: String, m: Row.ObjectTypeMap): RowValue[AnyRef]

  def refIndex(columnIndex: Int): RowValue[Ref]
  def refLabel(columnLabel: String): RowValue[Ref]

  def rowIdIndex(columnIndex: Int): RowValue[RowId]
  def rowIdLabel(columnLabel: String): RowValue[RowId]

  def shortIndex(columnIndex: Int): RowValue[Short]
  def shortLabel(columnLabel: String): RowValue[Short]

  def sqlxmlIndex(columnIndex: Int): RowValue[SQLXML]
  def sqlxmlLabel(columnLabel: String): RowValue[SQLXML]

  def stringIndex(columnIndex: Int): RowValue[String]
  def stringLabel(columnLabel: String): RowValue[String]

  def timeIndex(columnIndex: Int): RowValue[Time]
  def timeLabel(columnLabel: String): RowValue[Time]
  def timeIndexCal(columnIndex: Int, cal: Row.Cal): RowValue[Time]
  def timeLabelCal(columnLabel: String, cal: Row.Cal): RowValue[Time]

  def timestampIndex(columnIndex: Int): RowValue[Timestamp]
  def timestampLabel(columnLabel: String): RowValue[Timestamp]
  def timestampIndexCal(columnIndex: Int, cal: Row.Cal): RowValue[Timestamp]
  def timestampLabelCal(columnLabel: String, cal: Row.Cal): RowValue[Timestamp]

  def urlIndex(columnIndex: Int): RowValue[URL]
  def urlLabel(columnLabel: String): RowValue[URL]

  def keyLabel(label: String): RowValue[Key]
  def keyIndex(index: Int): RowValue[Key]

  def possibleKeyLabel(label: String): SqlValue[Key]
  def possibleKeyIndex(index: Int): SqlValue[Key]
}

object Row {
  import RowValue._
  import Key._

  type ObjectTypeMap = java.util.Map[String, Class[_]]
  type Cal = Calendar

  private[vault] def resultSetRow(r: ResultSet): Row = new Row {
    private def tryResultSet[A](a: => A): RowValue[A] =
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

    def arrayIndex(columnIndex: Int) =
      tryResultSet(r.getArray(columnIndex))
    def arrayLabel(columnLabel: String) =
      tryResultSet(r.getArray(columnLabel))

    def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnIndex)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnLabel)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def bigDecimalIndex(columnIndex: Int) =
      tryResultSet(r.getBigDecimal(columnIndex))
    def bigDecimalLabel(columnLabel: String) =
      tryResultSet(r.getBigDecimal(columnLabel))

    def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnIndex)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnLabel)
      try {
        tryResultSet(withInputStream(s))
      } finally {
        s.close
      }
    }

    def blobIndex(columnIndex: Int) =
      tryResultSet(r.getBlob(columnIndex))
    def blobLabel(columnLabel: String) =
      tryResultSet(r.getBlob(columnLabel))

    def booleanIndex(columnIndex: Int) =
      tryResultSet(r.getBoolean(columnIndex))
    def booleanLabel(columnLabel: String) =
      tryResultSet(r.getBoolean(columnLabel))

    def byteIndex(columnIndex: Int) =
      tryResultSet(r.getByte(columnIndex))
    def byteLabel(columnLabel: String) =
      tryResultSet(r.getByte(columnLabel))

    def bytesIndex(columnIndex: Int) =
      tryResultSet(r.getBytes(columnIndex))
    def bytesLabel(columnLabel: String) =
      tryResultSet(r.getBytes(columnLabel))

    def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnIndex)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def characterStreamLabel[A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnLabel)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def clobIndex(columnIndex: Int) =
      tryResultSet(r.getClob(columnIndex))
    def clobLabel(columnLabel: String) =
      tryResultSet(r.getClob(columnLabel))

    def dateIndex(columnIndex: Int) =
      tryResultSet(r.getDate(columnIndex))
    def dateLabel(columnLabel: String) =
      tryResultSet(r.getDate(columnLabel))
    def dateIndexCal(columnIndex: Int, cal: Cal) =
      tryResultSet(r.getDate(columnIndex, cal))
    def dateLabelCal(columnLabel: String, cal: Cal) =
      tryResultSet(r.getDate(columnLabel, cal))

    def doubleIndex(columnIndex: Int) =
      tryResultSet(r.getDouble(columnIndex))
    def doubleLabel(columnLabel: String) =
      tryResultSet(r.getDouble(columnLabel))

    def floatIndex(columnIndex: Int) =
      tryResultSet(r.getFloat(columnIndex))
    def floatLabel(columnLabel: String) =
      tryResultSet(r.getFloat(columnLabel))

    def intIndex(columnIndex: Int) =
      tryResultSet(r.getInt(columnIndex))
    def intLabel(columnLabel: String) =
      tryResultSet(r.getInt(columnLabel))

    def longIndex(columnIndex: Int) =
      tryResultSet(r.getLong(columnIndex))
    def longLabel(columnLabel: String) =
      tryResultSet(r.getLong(columnLabel))

    def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnIndex)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnLabel)
      try {
        tryResultSet(withReader(s))
      } finally {
        s.close
      }
    }

    def nclobIndex(columnIndex: Int) =
      tryResultSet(r.getNClob(columnIndex))
    def nclobLabel(columnLabel: String) =
      tryResultSet(r.getNClob(columnLabel))

    def nstringIndex(columnIndex: Int) =
      tryResultSet(r.getNString(columnIndex))
    def nstringLabel(columnLabel: String) =
      tryResultSet(r.getNString(columnLabel))

    def objectIndex(columnIndex: Int) =
      tryResultSet(r.getObject(columnIndex))
    def objectLabel(columnLabel: String) =
      tryResultSet(r.getObject(columnLabel))
    def objectMapIndex(columnIndex: Int, m: ObjectTypeMap) =
      tryResultSet(r.getObject(columnIndex, m))
    def objectMapLabel(columnLabel: String, m: ObjectTypeMap) =
      tryResultSet(r.getObject(columnLabel, m))

    def refIndex(columnIndex: Int) =
      tryResultSet(r.getRef(columnIndex))
    def refLabel(columnLabel: String) =
      tryResultSet(r.getRef(columnLabel))

    def rowIdIndex(columnIndex: Int) =
      tryResultSet(r.getRowId(columnIndex))
    def rowIdLabel(columnLabel: String) =
      tryResultSet(r.getRowId(columnLabel))

    def shortIndex(columnIndex: Int) =
      tryResultSet(r.getShort(columnIndex))
    def shortLabel(columnLabel: String) =
      tryResultSet(r.getShort(columnLabel))

    def sqlxmlIndex(columnIndex: Int) =
      tryResultSet(r.getSQLXML(columnIndex))
    def sqlxmlLabel(columnLabel: String) =
      tryResultSet(r.getSQLXML(columnLabel))

    def stringIndex(columnIndex: Int) =
      tryResultSet(r.getString(columnIndex))
    def stringLabel(columnLabel: String) =
      tryResultSet(r.getString(columnLabel))

    def timeIndex(columnIndex: Int) =
      tryResultSet(r.getTime(columnIndex))
    def timeLabel(columnLabel: String) =
      tryResultSet(r.getTime(columnLabel))
    def timeIndexCal(columnIndex: Int, cal: Cal) =
      tryResultSet(r.getTime(columnIndex, cal))
    def timeLabelCal(columnLabel: String, cal: Cal) =
      tryResultSet(r.getTime(columnLabel, cal))

    def timestampIndex(columnIndex: Int) =
      tryResultSet(r.getTimestamp(columnIndex))
    def timestampLabel(columnLabel: String) =
      tryResultSet(r.getTimestamp(columnLabel))
    def timestampIndexCal(columnIndex: Int, cal: Cal) =
      tryResultSet(r.getTimestamp(columnIndex))
    def timestampLabelCal(columnLabel: String, cal: Cal) =
      tryResultSet(r.getTimestamp(columnLabel))

    def urlIndex(columnIndex: Int) =
      tryResultSet(r.getURL(columnIndex))
    def urlLabel(columnLabel: String) =
      tryResultSet(r.getURL(columnLabel))

    def keyLabel(label: String) =
      longLabel(label) map (key(_))
    def keyIndex(index: Int) =
      longIndex(index) map (key(_))

    def possibleKeyLabel(label: String) =
      longLabel(label).possiblyNull map (_.toKey)

    def possibleKeyIndex(index: Int) =
      longIndex(index).possiblyNull map (_.toKey)
  }
}
