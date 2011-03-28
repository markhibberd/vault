package com.ephox.vault

import scalaz._
import Scalaz._
import java.io.{Reader, InputStream}
import java.util.Calendar
import java.sql.{Timestamp, Time, SQLXML, RowId, Ref, Date, Clob, Blob, ResultSet}
import java.net.URL

sealed trait Row {
  def iterate[L, A, T](a: RowAccess[L, A]): IterV[A, T] => RowValue[L, IterV[A, T]]

  def arrayIndex[L](columnIndex: Int): RowValue[L, java.sql.Array]
  def arrayLabel[L](columnLabel: String): RowValue[L, java.sql.Array]

  def asciiStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowValue[L, A]
  def asciiStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowValue[L, A]

  def bigDecimalIndex[L](columnIndex: Int): RowValue[L, java.math.BigDecimal]
  def bigDecimalLabel[L](columnLabel: String): RowValue[L, java.math.BigDecimal]

  def binaryStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A): RowValue[L, A]
  def binaryStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A): RowValue[L, A]

  def blobIndex[L](columnIndex: Int): RowValue[L, Blob]
  def blobLabel[L](columnLabel: String): RowValue[L, Blob]

  def booleanIndex[L](columnIndex: Int): RowValue[L, Boolean]
  def booleanLabel[L](columnLabel: String): RowValue[L, Boolean]

  def byteIndex[L](columnIndex: Int): RowValue[L, Byte]
  def byteLabel[L](columnLabel: String): RowValue[L, Byte]

  def bytesIndex[L](columnIndex: Int): RowValue[L, Array[Byte]]
  def bytesLabel[L](columnLabel: String): RowValue[L, Array[Byte]]

  def characterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowValue[L, A]
  def characterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowValue[L, A]

  def clobIndex[L](columnIndex: Int): RowValue[L, Clob]
  def clobLabel[L](columnLabel: String): RowValue[L, Clob]

  def dateIndex[L](columnIndex: Int): RowValue[L, Date]
  def dateLabel[L](columnLabel: String): RowValue[L, Date]
  def dateIndexCal[L](columnIndex: Int, cal: Row.Cal): RowValue[L, Date]
  def dateLabelCal[L](columnLabel: String, cal: Row.Cal): RowValue[L, Date]

  def doubleIndex[L](columnIndex: Int): RowValue[L, Double]
  def doubleLabel[L](columnLabel: String): RowValue[L, Double]

  def floatIndex[L](columnIndex: Int): RowValue[L, Float]
  def floatLabel[L](columnLabel: String): RowValue[L, Float]

  def intIndex[L](columnIndex: Int): RowValue[L, Int]
  def intLabel[L](columnLabel: String): RowValue[L, Int]

  def longIndex[L](columnIndex: Int): RowValue[L, Long]
  def longLabel[L](columnLabel: String): RowValue[L, Long]

  def ncharacterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A): RowValue[L, A]
  def ncharacterStreamLabel[L, A](columnLabel: String, withReader: Reader => A): RowValue[L, A]

  def nclobIndex[L](columnIndex: Int): RowValue[L, Clob]
  def nclobLabel[L](columnLabel: String): RowValue[L, Clob]

  def nstringIndex[L](columnIndex: Int): RowValue[L, String]
  def nstringLabel[L](columnLabel: String): RowValue[L, String]

  def objectIndex[L](columnIndex: Int): RowValue[L, AnyRef]
  def objectLabel[L](columnLabel: String): RowValue[L, AnyRef]
  def objectMapIndex[L](columnIndex: Int, m: Row.ObjectTypeMap): RowValue[L, AnyRef]
  def objectMapLabel[L](columnLabel: String, m: Row.ObjectTypeMap): RowValue[L, AnyRef]

  def refIndex[L](columnIndex: Int): RowValue[L, Ref]
  def refLabel[L](columnLabel: String): RowValue[L, Ref]

  def rowIdIndex[L](columnIndex: Int): RowValue[L, RowId]
  def rowIdLabel[L](columnLabel: String): RowValue[L, RowId]

  def shortIndex[L](columnIndex: Int): RowValue[L, Short]
  def shortLabel[L](columnLabel: String): RowValue[L, Short]

  def sqlxmlIndex[L](columnIndex: Int): RowValue[L, SQLXML]
  def sqlxmlLabel[L](columnLabel: String): RowValue[L, SQLXML]

  def stringIndex[L](columnIndex: Int): RowValue[L, String]
  def stringLabel[L](columnLabel: String): RowValue[L, String]

  def timeIndex[L](columnIndex: Int): RowValue[L, Time]
  def timeLabel[L](columnLabel: String): RowValue[L, Time]
  def timeIndexCal[L](columnIndex: Int, cal: Row.Cal): RowValue[L, Time]
  def timeLabelCal[L](columnLabel: String, cal: Row.Cal): RowValue[L, Time]

  def timestampIndex[L](columnIndex: Int): RowValue[L, Timestamp]
  def timestampLabel[L](columnLabel: String): RowValue[L, Timestamp]
  def timestampIndexCal[L](columnIndex: Int, cal: Row.Cal): RowValue[L, Timestamp]
  def timestampLabelCal[L](columnLabel: String, cal: Row.Cal): RowValue[L, Timestamp]

  def urlIndex[L](columnIndex: Int): RowValue[L, URL]
  def urlLabel[L](columnLabel: String): RowValue[L, URL]

  def keyLabel[L](label: String): RowValue[L, Key]
  def keyIndex[L](index: Int): RowValue[L, Key]

  // def possibleKeyLabel[L](label: String): RowValue[L, Key]
//  def possibleKeyIndex[L](index: Int): RowValue[L, Key] todo
}

object Row {
  type ObjectTypeMap = java.util.Map[String, Class[_]]
  type Cal = Calendar

  private[vault] def resultSetRow(r: ResultSet): Row = new Row {
    private def tryRowAccess[L, A](a: => A): RowValue[L, A] =
      try {
        // very dangerous, beware of effect on ResultSet (wasNull)
        val z = a
        if(r.wasNull) rowNull else z.η[({type λ[α]= RowValue[L, α]})#λ]
      } catch {
        case e: SqlException => rowError(e)
        case x => throw x
      }

    def iterate[L, A, T](ra: RowAccess[L, A]) =
      iter => {
        def loop(i: IterV[A, T]): RowValue[L, IterV[A, T]] =
          i.fold((a, ip) => i.η[({type λ[α]= RowValue[L, α]})#λ],
                 k => {
                   val hasMore = r.next
                   if (hasMore) ra.access(Row.resultSetRow(r)) flatMap (t => loop(k(IterV.El(t))))
                   else i.η[({type λ[α]= RowValue[L, α]})#λ]
                 })
        loop(iter)
      }

    def arrayIndex[L](columnIndex: Int) =
      tryRowAccess(r.getArray(columnIndex))
    def arrayLabel[L](columnLabel: String) =
      tryRowAccess(r.getArray(columnLabel))

    def asciiStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnIndex)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def asciiStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getAsciiStream(columnLabel)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def bigDecimalIndex[L](columnIndex: Int) =
      tryRowAccess(r.getBigDecimal(columnIndex))
    def bigDecimalLabel[L](columnLabel: String) =
      tryRowAccess(r.getBigDecimal(columnLabel))

    def binaryStreamIndex[L, A](columnIndex: Int, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnIndex)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def binaryStreamLabel[L, A](columnLabel: String, withInputStream: InputStream => A) = {
      val s = r.getBinaryStream(columnLabel)
      try {
        tryRowAccess(withInputStream(s))
      } finally {
        s.close
      }
    }

    def blobIndex[L](columnIndex: Int) =
      tryRowAccess(r.getBlob(columnIndex))
    def blobLabel[L](columnLabel: String) =
      tryRowAccess(r.getBlob(columnLabel))

    def booleanIndex[L](columnIndex: Int) =
      tryRowAccess(r.getBoolean(columnIndex))
    def booleanLabel[L](columnLabel: String) =
      tryRowAccess(r.getBoolean(columnLabel))

    def byteIndex[L](columnIndex: Int) =
      tryRowAccess(r.getByte(columnIndex))
    def byteLabel[L](columnLabel: String) =
      tryRowAccess(r.getByte(columnLabel))

    def bytesIndex[L](columnIndex: Int) =
      tryRowAccess(r.getBytes(columnIndex))
    def bytesLabel[L](columnLabel: String) =
      tryRowAccess(r.getBytes(columnLabel))

    def characterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnIndex)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def characterStreamLabel[L, A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getCharacterStream(columnLabel)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def clobIndex[L](columnIndex: Int) =
      tryRowAccess(r.getClob(columnIndex))
    def clobLabel[L](columnLabel: String) =
      tryRowAccess(r.getClob(columnLabel))

    def dateIndex[L](columnIndex: Int) =
      tryRowAccess(r.getDate(columnIndex))
    def dateLabel[L](columnLabel: String) =
      tryRowAccess(r.getDate(columnLabel))
    def dateIndexCal[L](columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getDate(columnIndex, cal))
    def dateLabelCal[L](columnLabel: String, cal: Cal) =
      tryRowAccess(r.getDate(columnLabel, cal))

    def doubleIndex[L](columnIndex: Int) =
      tryRowAccess(r.getDouble(columnIndex))
    def doubleLabel[L](columnLabel: String) =
      tryRowAccess(r.getDouble(columnLabel))

    def floatIndex[L](columnIndex: Int) =
      tryRowAccess(r.getFloat(columnIndex))
    def floatLabel[L](columnLabel: String) =
      tryRowAccess(r.getFloat(columnLabel))

    def intIndex[L](columnIndex: Int) =
      tryRowAccess(r.getInt(columnIndex))
    def intLabel[L](columnLabel: String) =
      tryRowAccess(r.getInt(columnLabel))

    def longIndex[L](columnIndex: Int) =
      tryRowAccess(r.getLong(columnIndex))
    def longLabel[L](columnLabel: String) =
      tryRowAccess(r.getLong(columnLabel))

    def ncharacterStreamIndex[L, A](columnIndex: Int, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnIndex)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def ncharacterStreamLabel[L, A](columnLabel: String, withReader: Reader => A) = {
      val s = r.getNCharacterStream(columnLabel)
      try {
        tryRowAccess(withReader(s))
      } finally {
        s.close
      }
    }

    def nclobIndex[L](columnIndex: Int) =
      tryRowAccess(r.getNClob(columnIndex))
    def nclobLabel[L](columnLabel: String) =
      tryRowAccess(r.getNClob(columnLabel))

    def nstringIndex[L](columnIndex: Int) =
      tryRowAccess(r.getNString(columnIndex))
    def nstringLabel[L](columnLabel: String) =
      tryRowAccess(r.getNString(columnLabel))

    def objectIndex[L](columnIndex: Int) =
      tryRowAccess(r.getObject(columnIndex))
    def objectLabel[L](columnLabel: String) =
      tryRowAccess(r.getObject(columnLabel))
    def objectMapIndex[L](columnIndex: Int, m: ObjectTypeMap) =
      tryRowAccess(r.getObject(columnIndex, m))
    def objectMapLabel[L](columnLabel: String, m: ObjectTypeMap) =
      tryRowAccess(r.getObject(columnLabel, m))

    def refIndex[L](columnIndex: Int) =
      tryRowAccess(r.getRef(columnIndex))
    def refLabel[L](columnLabel: String) =
      tryRowAccess(r.getRef(columnLabel))

    def rowIdIndex[L](columnIndex: Int) =
      tryRowAccess(r.getRowId(columnIndex))
    def rowIdLabel[L](columnLabel: String) =
      tryRowAccess(r.getRowId(columnLabel))

    def shortIndex[L](columnIndex: Int) =
      tryRowAccess(r.getShort(columnIndex))
    def shortLabel[L](columnLabel: String) =
      tryRowAccess(r.getShort(columnLabel))

    def sqlxmlIndex[L](columnIndex: Int) =
      tryRowAccess(r.getSQLXML(columnIndex))
    def sqlxmlLabel[L](columnLabel: String) =
      tryRowAccess(r.getSQLXML(columnLabel))

    def stringIndex[L](columnIndex: Int) =
      tryRowAccess(r.getString(columnIndex))
    def stringLabel[L](columnLabel: String) =
      tryRowAccess(r.getString(columnLabel))

    def timeIndex[L](columnIndex: Int) =
      tryRowAccess(r.getTime(columnIndex))
    def timeLabel[L](columnLabel: String) =
      tryRowAccess(r.getTime(columnLabel))
    def timeIndexCal[L](columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getTime(columnIndex, cal))
    def timeLabelCal[L](columnLabel: String, cal: Cal) =
      tryRowAccess(r.getTime(columnLabel, cal))

    def timestampIndex[L](columnIndex: Int) =
      tryRowAccess(r.getTimestamp(columnIndex))
    def timestampLabel[L](columnLabel: String) =
      tryRowAccess(r.getTimestamp(columnLabel))
    def timestampIndexCal[L](columnIndex: Int, cal: Cal) =
      tryRowAccess(r.getTimestamp(columnIndex))
    def timestampLabelCal[L](columnLabel: String, cal: Cal) =
      tryRowAccess(r.getTimestamp(columnLabel))

    def urlIndex[L](columnIndex: Int) =
      tryRowAccess(r.getURL(columnIndex))
    def urlLabel[L](columnLabel: String) =
      tryRowAccess(r.getURL(columnLabel))

    def keyLabel[L](label: String) =
      longLabel(label) map (Key.key(_))
    def keyIndex[L](index: Int) =
      longIndex(index) map (Key.key(_))
    /*
    def possibleKeyLabel[L](label: String) = longLabel(label).possiblyNull map ({
      case None => Key.nokey
      case Some(x) => Key.key(x)
    })

    def possibleKeyIndex[L](index: Int) = longIndex(index).possiblyNull map ({
      case None => Key.nokey
      case Some(x) => Key.key(x)
    })
    */
  }
}