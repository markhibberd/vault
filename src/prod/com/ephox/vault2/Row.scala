package com.ephox.vault2

import scalaz._
import Scalaz._
import java.io.{Reader, InputStream}
import java.util.Calendar
import java.sql.{Timestamp, Time, SQLXML, RowId, Ref, Date, Clob, Blob, SQLException, ResultSet}
import java.net.URL

sealed trait Row {
  def iterate[A, T](a: RowAccessor[A]): IterV[A, T] => RowAccess[IterV[A, T]]

  def arrayIndex(columnIndex: Int): RowAccess[java.sql.Array]
  def arrayLabel(columnLabel: String): RowAccess[java.sql.Array]

  def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[A]
  def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccess[A]

  def bigDecimalIndex(columnIndex: Int): RowAccess[java.math.BigDecimal]
  def bigDecimalLabel(columnLabel: String): RowAccess[java.math.BigDecimal]

  def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccess[A]
  def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccess[A]

  def blobIndex(columnIndex: Int): RowAccess[Blob]
  def blobLabel(columnLabel: String): RowAccess[Blob]

  def booleanIndex(columnIndex: Int): RowAccess[Boolean]
  def booleanLabel(columnLabel: String): RowAccess[Boolean]

  def byteIndex(columnIndex: Int): RowAccess[Byte]
  def byteLabel(columnLabel: String): RowAccess[Byte]

  def bytesIndex(columnIndex: Int): RowAccess[Array[Byte]]
  def bytesLabel(columnLabel: String): RowAccess[Array[Byte]]

  def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccess[A]
  def characterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccess[A]

  def clobIndex(columnIndex: Int): RowAccess[Clob]
  def clobLabel(columnLabel: String): RowAccess[Clob]

  def dateIndex(columnIndex: Int): RowAccess[Date]
  def dateLabel(columnLabel: String): RowAccess[Date]
  def dateIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Date]
  def dateLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Date]

  def doubleIndex(columnIndex: Int): RowAccess[Double]
  def doubleLabel(columnLabel: String): RowAccess[Double]

  def floatIndex(columnIndex: Int): RowAccess[Float]
  def floatLabel(columnLabel: String): RowAccess[Float]

  def intIndex(columnIndex: Int): RowAccess[Int]
  def intLabel(columnLabel: String): RowAccess[Int]

  def longIndex(columnIndex: Int): RowAccess[Long]
  def longLabel(columnLabel: String): RowAccess[Long]

  def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccess[A]
  def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccess[A]

  def nclobIndex(columnIndex: Int): RowAccess[Clob]
  def nclobLabel(columnLabel: String): RowAccess[Clob]

  def nstringIndex(columnIndex: Int): RowAccess[String]
  def nstringLabel(columnLabel: String): RowAccess[String]

  def objectIndex(columnIndex: Int): RowAccess[AnyRef]
  def objectLabel(columnLabel: String): RowAccess[AnyRef]
  def objectMapIndex(columnIndex: Int, m: Row.ObjectTypeMap): RowAccess[AnyRef]
  def objectMapLabel(columnLabel: String, m: Row.ObjectTypeMap): RowAccess[AnyRef]

  def refIndex(columnIndex: Int): RowAccess[Ref]
  def refLabel(columnLabel: String): RowAccess[Ref]

  def rowIdIndex(columnIndex: Int): RowAccess[RowId]
  def rowIdLabel(columnLabel: String): RowAccess[RowId]

  def shortIndex(columnIndex: Int): RowAccess[Short]
  def shortLabel(columnLabel: String): RowAccess[Short]

  def sqlxmlIndex(columnIndex: Int): RowAccess[SQLXML]
  def sqlxmlLabel(columnLabel: String): RowAccess[SQLXML]

  def stringIndex(columnIndex: Int): RowAccess[String]
  def stringLabel(columnLabel: String): RowAccess[String]

  def timeIndex(columnIndex: Int): RowAccess[Time]
  def timeLabel(columnLabel: String): RowAccess[Time]
  def timeIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Time]
  def timeLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Time]

  def timestampIndex(columnIndex: Int): RowAccess[Timestamp]
  def timestampLabel(columnLabel: String): RowAccess[Timestamp]
  def timestampIndexCal(columnIndex: Int, cal: Row.Cal): RowAccess[Timestamp]
  def timestampLabelCal(columnLabel: String, cal: Row.Cal): RowAccess[Timestamp]

  def urlIndex(columnIndex: Int): RowAccess[URL]
  def urlLabel(columnLabel: String): RowAccess[URL]
}

object Row {
  type ObjectTypeMap = java.util.Map[String, Class[_]]
  type Cal = Calendar

  private[vault2] def resultSetRow(r: ResultSet): Row = new Row {
    private def tryRowAccess[A](a: => A): RowAccess[A] =
      try {
        // very dangerous, beware of effect on ResultSet (wasNull)
        val z = a
        if(r.wasNull) rowAccessNull else z.η[RowAccess]
      } catch {
        case e: SQLException => rowAccessErr(e)
        case x => throw x
      }

    def iterate[A, T](ra: RowAccessor[A]) =
      iter => {
        def loop(i: IterV[A, T]): RowAccess[IterV[A, T]] =
          i.fold((a, ip) => i.η[RowAccess],
                 k => {
                   val hasMore = r.next
                   if (hasMore) ra.access(Row.resultSetRow(r)) flatMap (t => loop(k(IterV.El(t))))
                   else i.η[RowAccess]
                 })
        loop(iter)
      }

    def arrayIndex(columnIndex: Int) =
      tryRowAccess(r.getArray(columnIndex))
    def arrayLabel(columnLabel: String) =
      tryRowAccess(r.getArray(columnLabel))

    def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnIndex)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnLabel)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def bigDecimalIndex(columnIndex: Int) =
      tryRowAccess(r.getBigDecimal(columnIndex))
    def bigDecimalLabel(columnLabel: String) =
      tryRowAccess(r.getBigDecimal(columnLabel))

    def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnIndex)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnLabel)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def blobIndex(columnIndex: Int) =
      tryRowAccess(r.getBlob(columnIndex))
    def blobLabel(columnLabel: String) =
      tryRowAccess(r.getBlob(columnLabel))

    def booleanIndex(columnIndex: Int) =
      tryRowAccess(r.getBoolean(columnIndex))
    def booleanLabel(columnLabel: String) =
      tryRowAccess(r.getBoolean(columnLabel))

    def byteIndex(columnIndex: Int) =
      tryRowAccess(r.getByte(columnIndex))
    def byteLabel(columnLabel: String) =
      tryRowAccess(r.getByte(columnLabel))

    def bytesIndex(columnIndex: Int) =
      tryRowAccess(r.getBytes(columnIndex))
    def bytesLabel(columnLabel: String) =
      tryRowAccess(r.getBytes(columnLabel))

    def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnIndex)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def characterStreamLabel[A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnLabel)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def clobIndex(columnIndex: Int) =
      tryRowAccess(r.getClob(columnIndex))
    def clobLabel(columnLabel: String) =
      tryRowAccess(r.getClob(columnLabel))

    def dateIndex(columnIndex: Int) =
      tryRowAccess(r.getDate(columnIndex))
    def dateLabel(columnLabel: String) =
      tryRowAccess(r.getDate(columnLabel))
    def dateIndexCal(columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getDate(columnIndex, cal))
    def dateLabelCal(columnLabel: String, cal: Cal) =
      tryRowAccess(r.getDate(columnLabel, cal))

    def doubleIndex(columnIndex: Int) =
      tryRowAccess(r.getDouble(columnIndex))
    def doubleLabel(columnLabel: String) =
      tryRowAccess(r.getDouble(columnLabel))

    def floatIndex(columnIndex: Int) =
      tryRowAccess(r.getFloat(columnIndex))
    def floatLabel(columnLabel: String) =
      tryRowAccess(r.getFloat(columnLabel))

    def intIndex(columnIndex: Int) =
      tryRowAccess(r.getInt(columnIndex))
    def intLabel(columnLabel: String) =
      tryRowAccess(r.getInt(columnLabel))

    def longIndex(columnIndex: Int) =
      tryRowAccess(r.getLong(columnIndex))
    def longLabel(columnLabel: String) =
      tryRowAccess(r.getLong(columnLabel))

    def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnIndex)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnLabel)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def nclobIndex(columnIndex: Int) =
      tryRowAccess(r.getNClob(columnIndex))
    def nclobLabel(columnLabel: String) =
      tryRowAccess(r.getNClob(columnLabel))

    def nstringIndex(columnIndex: Int) =
      tryRowAccess(r.getNString(columnIndex))
    def nstringLabel(columnLabel: String) =
      tryRowAccess(r.getNString(columnLabel))

    def objectIndex(columnIndex: Int) =
      tryRowAccess(r.getObject(columnIndex))
    def objectLabel(columnLabel: String) =
      tryRowAccess(r.getObject(columnLabel))
    def objectMapIndex(columnIndex: Int, m: ObjectTypeMap) =
      tryRowAccess(r.getObject(columnIndex, m))
    def objectMapLabel(columnLabel: String, m: ObjectTypeMap) =
      tryRowAccess(r.getObject(columnLabel, m))

    def refIndex(columnIndex: Int) =
      tryRowAccess(r.getRef(columnIndex))
    def refLabel(columnLabel: String) =
      tryRowAccess(r.getRef(columnLabel))

    def rowIdIndex(columnIndex: Int) =
      tryRowAccess(r.getRowId(columnIndex))
    def rowIdLabel(columnLabel: String) =
      tryRowAccess(r.getRowId(columnLabel))

    def shortIndex(columnIndex: Int) =
      tryRowAccess(r.getShort(columnIndex))
    def shortLabel(columnLabel: String) =
      tryRowAccess(r.getShort(columnLabel))

    def sqlxmlIndex(columnIndex: Int) =
      tryRowAccess(r.getSQLXML(columnIndex))
    def sqlxmlLabel(columnLabel: String) =
      tryRowAccess(r.getSQLXML(columnLabel))

    def stringIndex(columnIndex: Int) =
      tryRowAccess(r.getString(columnIndex))
    def stringLabel(columnLabel: String) =
      tryRowAccess(r.getString(columnLabel))

    def timeIndex(columnIndex: Int) =
      tryRowAccess(r.getTime(columnIndex))
    def timeLabel(columnLabel: String) =
      tryRowAccess(r.getTime(columnLabel))
    def timeIndexCal(columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getTime(columnIndex, cal))
    def timeLabelCal(columnLabel: String, cal: Cal) =
      tryRowAccess(r.getTime(columnLabel, cal))

    def timestampIndex(columnIndex: Int) =
      tryRowAccess(r.getTimestamp(columnIndex))
    def timestampLabel(columnLabel: String) =
      tryRowAccess(r.getTimestamp(columnLabel))
    def timestampIndexCal(columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getTimestamp(columnIndex))
    def timestampLabelCal(columnLabel: String, cal: Cal) =
      tryRowAccess(r.getTimestamp(columnLabel))

    def urlIndex(columnIndex: Int) =
      tryRowAccess(r.getURL(columnIndex))
    def urlLabel(columnLabel: String) =
      tryRowAccess(r.getURL(columnLabel))
  }
}