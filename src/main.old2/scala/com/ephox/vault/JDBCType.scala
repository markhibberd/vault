package com.ephox
package vault

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

object JDBCType extends JDBCTypeFunctions with JDBCTypeInstances

trait JDBCTypeFunctions {
  /**
   * Construct a `null` type.
   */
  val nullType: SqlType => JDBCType = NullJDBCType(_)

  /**
   * Construct a `Boolean` type.
   */
  val booleanType: Boolean => JDBCType = BooleanJDBCType(_)

  /**
   * Construct a `Byte` type.
   */
  val byteType: Byte => JDBCType = ByteJDBCType(_)

  /**
   * Construct a `Short` type.
   */
  val shortType: Short => JDBCType = ShortJDBCType(_)

  /**
   * Construct a `Int` type.
   */
  val intType: Int => JDBCType = IntJDBCType(_)

  /**
   * Construct a `Long` type.
   */
  val longType: Long => JDBCType = LongJDBCType(_)

  /**
   * Construct a `Float` type.
   */
  val floatType: Float => JDBCType = FloatJDBCType(_)

  /**
   * Construct a `Double` type.
   */
  val doubleType: Double => JDBCType = DoubleJDBCType(_)

  /**
   * Construct a `BigDecimal` type.
   */
  val bigDecimalType: java.math.BigDecimal => JDBCType = BigDecimalJDBCType(_)

  /**
   * Construct a `String` type.
   */
  val stringType: String => JDBCType = StringJDBCType(_)

  /**
   * Construct an `Array[Byte]` type.
   */
  val bytesType: Array[Byte] => JDBCType = BytesJDBCType(_)

  /**
   * Construct a `Date` type.
   */
  val dateType: Date => JDBCType = DateJDBCType(_)

  /**
   * Construct a `Time` type.
   */
  val timeType: Time => JDBCType = TimeJDBCType(_)

  /**
   * Construct a `Timestamp` type.
   */
  val timestampType: Timestamp => JDBCType = TimestampJDBCType(_)

  /**
   * Construct an ASCII `InputStream` type taking the given number of bytes.
   */
  val asciiStreamType: (InputStream, Int) => JDBCType = AsciiStreamJDBCType(_, _)

  /**
   * Construct a binary `InputStream` type taking the given number of bytes.
   */
  val binaryStreamType: (InputStream, Int) => JDBCType = BinaryStreamJDBCType(_, _)

  /**
   * Construct an 'Object` with a specific sql target type.
   */
  val objectTypeType: (AnyRef, SqlType) => JDBCType = ObjectTypeJDBCType(_, _)

  /**
   * Construct an `Object` type.
   */
  val objectType: AnyRef => JDBCType = ObjectJDBCType(_)

  /**
   * Construct a `Reader` type taking the given number of characters.
   */
  val characterStreamType: (Reader, Int) => JDBCType = CharacterStreamJDBCType(_, _)

  /**
   * Construct a `Ref` type.
   */
  val refType: Ref => JDBCType = RefJDBCType(_)

  /**
   * Construct a `Blob` type.
   */
  val blobType: Blob => JDBCType = BlobJDBCType(_)

  /**
   * Construct a `Clob` type.
   */
  val clobType: Clob => JDBCType = ClobJDBCType(_)

  /**
   * Construct an sql `Array` type.
   */
  val arrayType: java.sql.Array => JDBCType = ArrayJDBCType(_)

  /**
   * Construct a `Date` type with a specific `Calendar`.
   */
  val calendarDateType: (Date, Calendar) => JDBCType = CalendarDateJDBCType(_, _)

  /**
   * Construct a `Time` type with a specific `Calendar`.
   */
  val calendarTimeType: (Time, Calendar) => JDBCType = CalendarTimeJDBCType(_, _)

  /**
   * Construct a `Timestamp` type with a specific `Calendar`.
   */
  val calendarTimestampType: (Timestamp, Calendar) => JDBCType = CalendarTimestampJDBCType(_, _)

  /**
   * Construct a `null` type with a fully-qualified name of an SQL user-defined type.
   */
  val userNullType: (SqlType, String) => JDBCType = UserNullJDBCType(_, _)

  /**
   * Construct a `URL` type.
   */
  val urlType: URL => JDBCType = URLJDBCType(_)

  /**
   * A `null` type partial lens.
   */
  val nullTypeL: JDBCType @?> SqlType =
    PLens {
      case NullJDBCType(a) => Some(Store(NullJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Boolean` type partial lens.
   */
  val booleanTypeL: JDBCType @?> Boolean =
    PLens {
      case BooleanJDBCType(a) => Some(Store(BooleanJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Byte` type partial lens.
   */
  val byteTypeL: JDBCType @?> Byte =
    PLens {
      case ByteJDBCType(a) => Some(Store(ByteJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Short` type partial lens.
   */
  val shortTypeL: JDBCType @?> Short =
    PLens {
      case ShortJDBCType(a) => Some(Store(ShortJDBCType(_), a))
      case _ => None
    }

  /**
   * An `Int` type partial lens.
   */
  val intTypeL: JDBCType @?> Int =
    PLens {
      case IntJDBCType(a) => Some(Store(IntJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Long` type partial lens.
   */
  val longTypeL: JDBCType @?> Long =
    PLens {
      case IntJDBCType(a) => Some(Store(LongJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Float` type partial lens.
   */
  val floatTypeL: JDBCType @?> Float =
    PLens {
      case FloatJDBCType(a) => Some(Store(FloatJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Double` type partial lens.
   */
  val doubleTypeL: JDBCType @?> Double =
    PLens {
      case DoubleJDBCType(a) => Some(Store(DoubleJDBCType(_), a))
      case _ => None
    }

  /**
   * A `BigDecimal` type partial lens.
   */
  val bigDecimalTypeL: JDBCType @?> java.math.BigDecimal =
    PLens {
      case BigDecimalJDBCType(a) => Some(Store(BigDecimalJDBCType(_), a))
      case _ => None
    }

  /**
   * A `String` type partial lens.
   */
  val stringTypeL: JDBCType @?> String =
    PLens {
      case StringJDBCType(a) => Some(Store(StringJDBCType(_), a))
      case _ => None
    }

  /**
   * An `Array[Byte]` type partial lens.
   */
  val bytesTypeL: JDBCType @?> Array[Byte] =
    PLens {
      case BytesJDBCType(a) => Some(Store(BytesJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Date` type partial lens.
   */
  val dateTypeL: JDBCType @?> Date =
    PLens {
      case DateJDBCType(a) => Some(Store(DateJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Time` type partial lens.
   */
  val timeTypeL: JDBCType @?> Time =
    PLens {
      case TimeJDBCType(a) => Some(Store(TimeJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Timestamp` type partial lens.
   */
  val timestampTypeL: JDBCType @?> Timestamp =
    PLens {
      case TimestampJDBCType(a) => Some(Store(TimestampJDBCType(_), a))
      case _ => None
    }

  /**
   * An ASCII `InputStream` type partial lens.
   */
  val asciiStreamTypeL: JDBCType @?> (InputStream, Int) =
    PLens {
      case AsciiStreamJDBCType(a, b) => Some(Store(x => AsciiStreamJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A binary `InputStream` type partial lens.
   */
  val binaryStreamTypeL: JDBCType @?> (InputStream, Int) =
    PLens {
      case BinaryStreamJDBCType(a, b) => Some(Store(x => BinaryStreamJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * An object target type partial lens.
   */
  val objectTypeTypeL: JDBCType @?> (AnyRef, SqlType) =
    PLens {
      case ObjectTypeJDBCType(a, b) => Some(Store(x => ObjectTypeJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * An `object` type partial lens.
   */
  val objectTypeL: JDBCType @?> AnyRef =
    PLens {
      case ObjectJDBCType(a) => Some(Store(ObjectJDBCType(_), a))
      case _ => None
    }

  /**
   * A character stream type partial lens.
   */
  val characterStreamTypeL: JDBCType @?> (Reader, Int) =
    PLens {
      case CharacterStreamJDBCType(a, b) => Some(Store(x => CharacterStreamJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A `Ref` type partial lens.
   */
  val refTypeL: JDBCType @?> Ref =
    PLens {
      case RefJDBCType(a) => Some(Store(RefJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Blob` type partial lens.
   */
  val blobTypeL: JDBCType @?> Blob =
    PLens {
      case BlobJDBCType(a) => Some(Store(BlobJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Clob` type partial lens.
   */
  val clobTypeL: JDBCType @?> Clob =
    PLens {
      case ClobJDBCType(a) => Some(Store(ClobJDBCType(_), a))
      case _ => None
    }

  /**
   * A sql `Array` type partial lens.
   */
  val arrayTypeL: JDBCType @?> java.sql.Array =
    PLens {
      case ArrayJDBCType(a) => Some(Store(ArrayJDBCType(_), a))
      case _ => None
    }

  /**
   * A `Calendar` and `Date` type partial lens.
   */
  val calendarDateTypeL: JDBCType @?> (Date, Calendar) =
    PLens {
      case CalendarDateJDBCType(a, b) => Some(Store(x => CalendarDateJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A `Calendar` and `Time` type partial lens.
   */
  val calendarTimeTypeL: JDBCType @?> (Time, Calendar) =
    PLens {
      case CalendarTimeJDBCType(a, b) => Some(Store(x => CalendarTimeJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A `Calendar` and `Timestamp` type partial lens.
   */
  val calendarTimestampTypeL: JDBCType @?> (Timestamp, Calendar) =
    PLens {
      case CalendarTimestampJDBCType(a, b) => Some(Store(x => CalendarTimestampJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A `SqlType` and `String` type partial lens.
   */
  val userNullTypeL: JDBCType @?> (SqlType, String) =
    PLens {
      case UserNullJDBCType(a, b) => Some(Store(x => UserNullJDBCType(x._1, x._2), (a, b)))
      case _ => None
    }

  /**
   * A `URL` type partial lens.
   */
  val urlTypeL: JDBCType @?> URL =
    PLens {
      case URLJDBCType(a) => Some(Store(URLJDBCType(_), a))
      case _ => None
    }
}

trait JDBCTypeInstances {

  implicit val JDBCTypeShow: Show[JDBCType] =
    Show.showA
}
