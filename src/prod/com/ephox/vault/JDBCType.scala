package com.ephox.vault

import java.util.Calendar
import java.io.{Reader, InputStream}
import java.net.URL
import java.sql.{Clob, Blob, Ref, Timestamp, Time, Date}

sealed trait JDBCType {
  def fold[X](
               nullType: SqlType => X
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
             , objectTypeType: AnyRef => SqlType => X
             , objectType: AnyRef => X
             , characterStreamType: Reader => Int => X
             , refType: Ref => X
             , blobType: Blob => X
             , clobType: Clob => X
             , arrayType: java.sql.Array => X
             , calendarDateType: Date => Calendar => X
             , calendarTimeType: Time => Calendar => X
             , calendarTimestampType: Timestamp => Calendar => X
             , userNullType: SqlType => String => X
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
private case class NullJDBCType(typ: SqlType) extends JDBCType
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
private case class ObjectTypeJDBCType(value: AnyRef, typ: SqlType) extends JDBCType
private case class ObjectJDBCType(value: AnyRef) extends JDBCType
private case class CharacterStreamJDBCType(value: Reader, length: Int) extends JDBCType
private case class RefJDBCType(value: Ref) extends JDBCType
private case class BlobJDBCType(value: Blob) extends JDBCType
private case class ClobJDBCType(value: Clob) extends JDBCType
private case class ArrayJDBCType(value: java.sql.Array) extends JDBCType
private case class CalendarDateJDBCType(value: Date, cal: Calendar) extends JDBCType
private case class CalendarTimeJDBCType(value: Time, cal: Calendar) extends JDBCType
private case class CalendarTimestampJDBCType(value: Timestamp, cal: Calendar) extends JDBCType
private case class UserNullJDBCType(typ: SqlType, name: String) extends JDBCType
private case class URLJDBCType(value: URL) extends JDBCType

trait JDBCTypes {
  def nullType: SqlType => JDBCType = NullJDBCType(_)
  def booleanType: Boolean => JDBCType = BooleanJDBCType(_)
  def byteType: Byte => JDBCType = ByteJDBCType(_)
  def shortType: Short => JDBCType = ShortJDBCType(_)
  def intType: Int => JDBCType = IntJDBCType(_)
  def longType: Long => JDBCType = LongJDBCType(_)
  def floatType: Float => JDBCType = FloatJDBCType(_)
  def doubleType: Double => JDBCType = DoubleJDBCType(_)
  def bigDecimalType: java.math.BigDecimal => JDBCType = BigDecimalJDBCType(_)
  def stringType: String => JDBCType = StringJDBCType(_)
  def bytesType: Array[Byte] => JDBCType = BytesJDBCType(_)
  def dateType: Date => JDBCType = DateJDBCType(_)
  def timeType: Time => JDBCType = TimeJDBCType(_)
  def timestampType: Timestamp => JDBCType = TimestampJDBCType(_)
  def asciiStreamType: (InputStream, Int) => JDBCType = AsciiStreamJDBCType(_, _)
  def unicodeStreamType: (InputStream, Int) => JDBCType = UnicodeStreamJDBCType(_, _)
  def binaryStreamType: (InputStream, Int) => JDBCType = BinaryStreamJDBCType(_, _)
  def objectTypeType: (AnyRef, SqlType) => JDBCType = ObjectTypeJDBCType(_, _)
  def objectType: AnyRef => JDBCType = ObjectJDBCType(_)
  def characterStreamType: (Reader, Int) => JDBCType = CharacterStreamJDBCType(_, _)
  def refType: Ref => JDBCType = RefJDBCType(_)
  def blobType: Blob => JDBCType = BlobJDBCType(_)
  def clobType: Clob => JDBCType = ClobJDBCType(_)
  def arrayType: java.sql.Array => JDBCType = ArrayJDBCType(_)
  def calendarDateType: (Date, Calendar) => JDBCType = CalendarDateJDBCType(_, _)
  def calendarTimeType: (Time, Calendar) => JDBCType = CalendarTimeJDBCType(_, _)
  def calendarTimestampType: (Timestamp, Calendar) => JDBCType = CalendarTimestampJDBCType(_, _)
  def userNullType: (SqlType, String) => JDBCType = UserNullJDBCType(_, _)
  def urlType: URL => JDBCType = URLJDBCType(_)
}
