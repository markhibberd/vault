package com.ephox.vault

import java.util.Calendar
import java.io.{Reader, InputStream}
import java.net.URL
import java.sql.{Clob, Blob, Ref, Timestamp, Time, Date}

sealed trait JDBCType {
  def fold[X](
               nullType: SQLType => X
             , booleanType: Boolean => X
             , byteType: Byte => X
             , shortType: Short => X
             , intType: Int => X
             , longType: Long => X
             , floatType: Float => X
             , doubleType: Double => X
             , bigDecimalType: java.math.BigDecimal => X
             , stringType: String => X
             , bytesType: Array[Byte] => X
             , dateType: Date => X
             , timeType: Time => X
             , timestampType: Timestamp => X
             , asciiStreamType: InputStream => Int => X
             , unicodeStreamType: InputStream => Int => X
             , binaryStreamType: InputStream => Int => X
             , objectTypeType: AnyRef => SQLType => X
             , objectType: AnyRef => X
             , characterStreamType: Reader => Int => X
             , refType: Ref => X
             , blobType: Blob => X
             , clobType: Clob => X
             , arrayType: java.sql.Array => X
             , calendarDateType: Date => Calendar => X
             , calendarTimeType: Time => Calendar => X
             , calendarTimestampType: Timestamp => Calendar => X
             , userNullType: SQLType => String => X
             , urlType: URL => X
             ) =
  this match {
    case NullJDBCType(typ) => nullType(typ)
    case BooleanJDBCType(value) => booleanType(value)
    case ByteJDBCType(value) => byteType(value)
    case ShortJDBCType(value) => shortType(value)
    case IntJDBCType(value) => intType(value)
    case LongJDBCType(value) => longType(value)
    case FloatJDBCType(value) => floatType(value)
    case DoubleJDBCType(value) => doubleType(value)
    case BigDecimalJDBCType(value) => bigDecimalType(value)
    case StringJDBCType(value) => stringType(value)
    case BytesJDBCType(value) => bytesType(value)
    case DateJDBCType(value) => dateType(value)
    case TimeJDBCType(value) => timeType(value)
    case TimestampJDBCType(value) => timestampType(value)
    case AsciiStreamJDBCType(value, length) => asciiStreamType(value)(length)
    case UnicodeStreamJDBCType(value, length) => unicodeStreamType(value)(length)
    case BinaryStreamJDBCType(value, length) => binaryStreamType(value)(length)
    case ObjectTypeJDBCType(value, typ) => objectTypeType(value)(typ)
    case ObjectJDBCType(value) => objectType(value)
    case CharacterStreamJDBCType(value, length) => characterStreamType(value)(length)
    case RefJDBCType(value) => refType(value)
    case BlobJDBCType(value) => blobType(value)
    case ClobJDBCType(value) => clobType(value)
    case ArrayJDBCType(value) => arrayType(value)
    case CalendarDateJDBCType(value, cal) => calendarDateType(value)(cal)
    case CalendarTimeJDBCType(value, cal) => calendarTimeType(value)(cal)
    case CalendarTimestampJDBCType(value, cal) => calendarTimestampType(value)(cal)
    case UserNullJDBCType(typ, name) => userNullType(typ)(name)
    case URLJDBCType(value) => urlType(value)
  }
}
private case class NullJDBCType(typ: SQLType) extends JDBCType
private case class BooleanJDBCType(value: Boolean) extends JDBCType
private case class ByteJDBCType(value: Byte) extends JDBCType
private case class ShortJDBCType(value: Short) extends JDBCType
private case class IntJDBCType(value: Int) extends JDBCType
private case class LongJDBCType(value: Long) extends JDBCType
private case class FloatJDBCType(value: Float) extends JDBCType
private case class DoubleJDBCType(value: Double) extends JDBCType
private case class BigDecimalJDBCType(value: java.math.BigDecimal) extends JDBCType
private case class StringJDBCType(value: String) extends JDBCType
private case class BytesJDBCType(value: Array[Byte]) extends JDBCType
private case class DateJDBCType(value: Date) extends JDBCType
private case class TimeJDBCType(value: Time) extends JDBCType
private case class TimestampJDBCType(value: Timestamp) extends JDBCType
private case class AsciiStreamJDBCType(value: InputStream, length: Int) extends JDBCType
private case class UnicodeStreamJDBCType(value: InputStream, length: Int) extends JDBCType
private case class BinaryStreamJDBCType(value: InputStream, length: Int) extends JDBCType
private case class ObjectTypeJDBCType(value: AnyRef, typ: SQLType) extends JDBCType
private case class ObjectJDBCType(value: AnyRef) extends JDBCType
private case class CharacterStreamJDBCType(value: Reader, length: Int) extends JDBCType
private case class RefJDBCType(value: Ref) extends JDBCType
private case class BlobJDBCType(value: Blob) extends JDBCType
private case class ClobJDBCType(value: Clob) extends JDBCType
private case class ArrayJDBCType(value: java.sql.Array) extends JDBCType
private case class CalendarDateJDBCType(value: Date, cal: Calendar) extends JDBCType
private case class CalendarTimeJDBCType(value: Time, cal: Calendar) extends JDBCType
private case class CalendarTimestampJDBCType(value: Timestamp, cal: Calendar) extends JDBCType
private case class UserNullJDBCType(typ: SQLType, name: String) extends JDBCType
private case class URLJDBCType(value: URL) extends JDBCType

object JDBCType {
  def nullType(typ: SQLType): JDBCType = NullJDBCType(typ)
  def booleanType(value: Boolean): JDBCType = BooleanJDBCType(value)
  def byteType(value: Byte): JDBCType = ByteJDBCType(value)
  def shortType(value: Short): JDBCType = ShortJDBCType(value)
  def intType(value: Int): JDBCType = IntJDBCType(value)
  def longType(value: Long): JDBCType = LongJDBCType(value)
  def floatType(value: Float): JDBCType = FloatJDBCType(value)
  def doubleType(value: Double): JDBCType = DoubleJDBCType(value)
  def bigDecimalType(value: java.math.BigDecimal): JDBCType = BigDecimalJDBCType(value)
  def stringType(value: String): JDBCType = StringJDBCType(value)
  def bytesType(value: Array[Byte]): JDBCType = BytesJDBCType(value)
  def dateType(value: Date): JDBCType = DateJDBCType(value)
  def timeType(value: Time): JDBCType = TimeJDBCType(value)
  def timestampType(value: Timestamp): JDBCType = TimestampJDBCType(value)
  def asciiStreamType(value: InputStream, length: Int): JDBCType = AsciiStreamJDBCType(value, length)
  def unicodeStreamType(value: InputStream, length: Int): JDBCType = UnicodeStreamJDBCType(value, length)
  def binaryStreamType(value: InputStream, length: Int): JDBCType = BinaryStreamJDBCType(value, length)
  def objectTypeType(value: AnyRef, typ: SQLType): JDBCType = ObjectTypeJDBCType(value, typ)
  def objectType(value: AnyRef): JDBCType = ObjectJDBCType(value)
  def characterStreamType(value: Reader, length: Int): JDBCType = CharacterStreamJDBCType(value, length)
  def refType(value: Ref): JDBCType = RefJDBCType(value)
  def blobType(value: Blob): JDBCType = BlobJDBCType(value)
  def clobType(value: Clob): JDBCType = ClobJDBCType(value)
  def arrayType(value: java.sql.Array): JDBCType = ArrayJDBCType(value)
  def calendarDateType(value: Date, cal: Calendar): JDBCType = CalendarDateJDBCType(value, cal)
  def calendarTimeType(value: Time, cal: Calendar): JDBCType = CalendarTimeJDBCType(value, cal)
  def calendarTimestampType(value: Timestamp, cal: Calendar): JDBCType = CalendarTimestampJDBCType(value, cal)
  def userNullType(typ: SQLType, name: String): JDBCType = UserNullJDBCType(typ, name)
  def urlType(value: URL): JDBCType = URLJDBCType(value)
}
