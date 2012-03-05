package com.ephox.vault

import java.util.Calendar
import java.io.{Reader, InputStream}
import java.net.URL
import java.sql.{Clob, Blob, Ref, Timestamp, Time, Date}
import scalaz._, Scalaz._

/**
 * Denotes all the different types that can be set on a SQL statement (compound disjoint union).
 *
 * @see java.sql.PreparedStatement
 * @author Tony Morris <tmorris@tmorris.net>
 */
sealed trait JDBCType {
  /**
   * Reduction (catamorphism) on the different JDBC types.
   *
   * @tparam X The return type used in picking one (and only one) of the given reduction functions.
   * @param nullType If the type is `null` with a specific sql-type.
   * @param booleanType If the type is `Boolean`.
   * @param byteType If the type is `Byte`.
   * @param shortType If the type is `Short`.
   * @param intType If the type is `Int`.
   * @param longType If the type is `Long`.
   * @param floatType If the type is `Float`.
   * @param doubleType If the type is `Double`.
   * @param bigDecimalType If the type is `BigDecimal`.
   * @param stringType If the type is `String`.
   * @param bytesType If the type is `Array[Byte]`.
   * @param dateType If the type is `Date`.
   * @param timeType If the type is `Time`.
   * @param timestampType If the type is `Timestamp`.
   * @param asciiStreamType If the type is an ASCII `InputStream` taking a specific number of bytes.
   * @param binaryStreamType If the type is a binary `InputStream` taking a specific number of bytes.
   * @param objectTypeType If the type is an 'Object` with a specific sql target type.
   * @param objectType If the type is 'Object`.
   * @param characterStreamType If the type is a `Reader` taking a specific number of characters.
   * @param refType If the type is `Ref`.
   * @param blobType If the type is `Blob`.
   * @param clobType If the type is `Clob`.
   * @param arrayType If the type is sql `Array`.
   * @param calendarDateType If the type is `Date` with a specific `Calendar`.
   * @param calendarTimeType If the type is `Time` with a specific `Calendar`.
   * @param calendarTimestampType If the type is `Timestamp` with a specific `Calendar`.
   * @param userNullType If the type is `null` with a fully-qualified name of an SQL user-defined type.
   * @param urlType If the type is `URL`.
   * @return The result of reduction on one and only one of the given function values.
   */
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

object JDBCType extends JDBCTypes

trait JDBCTypes {
  /**
   * Construct a `null` type.
   */
  def nullType: SqlType => JDBCType = NullJDBCType(_)

  /**
   * Construct a `Boolean` type.
   */
  def booleanType: Boolean => JDBCType = BooleanJDBCType(_)

  /**
   * Construct a `Byte` type.
   */
  def byteType: Byte => JDBCType = ByteJDBCType(_)

  /**
   * Construct a `Short` type.
   */
  def shortType: Short => JDBCType = ShortJDBCType(_)

  /**
   * Construct a `Int` type.
   */
  def intType: Int => JDBCType = IntJDBCType(_)

  /**
   * Construct a `Long` type.
   */
  def longType: Long => JDBCType = LongJDBCType(_)

  /**
   * Construct a `Float` type.
   */
  def floatType: Float => JDBCType = FloatJDBCType(_)

  /**
   * Construct a `Double` type.
   */
  def doubleType: Double => JDBCType = DoubleJDBCType(_)

  /**
   * Construct a `BigDecimal` type.
   */
  def bigDecimalType: java.math.BigDecimal => JDBCType = BigDecimalJDBCType(_)

  /**
   * Construct a `String` type.
   */
  def stringType: String => JDBCType = StringJDBCType(_)

  /**
   * Construct an `Array[Byte]` type.
   */
  def bytesType: Array[Byte] => JDBCType = BytesJDBCType(_)

  /**
   * Construct a `Date` type.
   */
  def dateType: Date => JDBCType = DateJDBCType(_)

  /**
   * Construct a `Time` type.
   */
  def timeType: Time => JDBCType = TimeJDBCType(_)

  /**
   * Construct a `Timestamp` type.
   */
  def timestampType: Timestamp => JDBCType = TimestampJDBCType(_)

  /**
   * Construct an ASCII `InputStream` type taking the given number of bytes.
   */
  def asciiStreamType: (InputStream, Int) => JDBCType = AsciiStreamJDBCType(_, _)

  /**
   * Construct a binary `InputStream` type taking the given number of bytes.
   */
  def binaryStreamType: (InputStream, Int) => JDBCType = BinaryStreamJDBCType(_, _)

  /**
   * Construct an 'Object` with a specific sql target type.
   */
  def objectTypeType: (AnyRef, SqlType) => JDBCType = ObjectTypeJDBCType(_, _)

  /**
   * Construct an `Object` type.
   */
  def objectType: AnyRef => JDBCType = ObjectJDBCType(_)

  /**
   * Construct a `Reader` type taking the given number of characters.
   */
  def characterStreamType: (Reader, Int) => JDBCType = CharacterStreamJDBCType(_, _)

  /**
   * Construct a `Ref` type.
   */
  def refType: Ref => JDBCType = RefJDBCType(_)

  /**
   * Construct a `Blob` type.
   */
  def blobType: Blob => JDBCType = BlobJDBCType(_)

  /**
   * Construct a `Clob` type.
   */
  def clobType: Clob => JDBCType = ClobJDBCType(_)

  /**
   * Construct an sql `Array` type.
   */
  def arrayType: java.sql.Array => JDBCType = ArrayJDBCType(_)

  /**
   * Construct a `Date` type with a specific `Calendar`.
   */
  def calendarDateType: (Date, Calendar) => JDBCType = CalendarDateJDBCType(_, _)

  /**
   * Construct a `Time` type with a specific `Calendar`.
   */
  def calendarTimeType: (Time, Calendar) => JDBCType = CalendarTimeJDBCType(_, _)

  /**
   * Construct a `Timestamp` type with a specific `Calendar`.
   */
  def calendarTimestampType: (Timestamp, Calendar) => JDBCType = CalendarTimestampJDBCType(_, _)

  /**
   * Construct a `null` type with a fully-qualified name of an SQL user-defined type.
   */
  def userNullType: (SqlType, String) => JDBCType = UserNullJDBCType(_, _)

  /**
   * Construct a `URL` type.
   */
  def urlType: URL => JDBCType = URLJDBCType(_)

  implicit val JDBCTypeShow: Show[JDBCType] = showA
}
