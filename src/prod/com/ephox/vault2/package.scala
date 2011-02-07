package com.ephox

import scalaz._
import Scalaz._
import vault2._
import java.io.{Reader, InputStream}
import java.sql.{Date, Clob, Blob, Ref, Timestamp, Time, PreparedStatement, ResultSet, SQLException, Connection}
import java.net.URL
import java.util.Calendar

package object vault2 {
  implicit def StringStringQuery(s: String): StringQuery =
    StringQuery.stringQuery(s)

  implicit def StringQueryString(sql: StringQuery): String =
    sql.sql

  implicit def PreparedStatementPreparedStatementW(t: PreparedStatement): PreparedStatementW =
    PreparedStatementW.PreparedStatementPreparedStatementW(t)

  implicit def PreparedStatementWPreparedStatement(t: PreparedStatementW): PreparedStatement =
    t.s

  def sqlErr[A](e: SQLException): SQLValue[A] =
    SQLValue.err(e)

  // alias for η specialised to SQLValue
  def sqlValue[A](v: A): SQLValue[A] =
    SQLValue.value(v)

  def tryValue[A](a: => A): SQLValue[A] =
    try {
      a.η[SQLValue]
    } catch {
      case e: SQLException => sqlErr(e)
      case e               => throw e
    }

  def rowAccessValue[A](a: SQLValue[A]): RowAccess[A] =
    a.fold(RowAccess.err(_), RowAccess.value(_))

  def withSQLResource[T, R](
                          value: => T
                        , evaluate: T => SQLValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SQLValue[R] =
    withResource(value, evaluate, {
      case e: SQLException => sqlErr(e)
      case e               => throw e
    }, whenClosing)

  def connector[A](f: Connection => SQLValue[A]): Connector[A] =
    Connector.connector(f)

  def constantConnector[A](v: => SQLValue[A]): Connector[A] =
    connector(_ => v)

  def valueConnector[A](f: Connection => A): Connector[A] =
    connector(f(_).η[SQLValue])

  def tryConnector[A](f: Connection => A): Connector[A] =
    connector(c => tryValue(f(c)))

  val close: Connector[Unit] =
    tryConnector(_.close)

  def resultSetConnector[A](f: ResultSet => Connector[A]): ResultSetConnector[A] =
    ResultSetConnector.resultSetConnector(f)

  def tryResultSetConnector[A](f: ResultSet => Connection => A): ResultSetConnector[A] =
    resultSetConnector((r: ResultSet) => tryConnector(c => f(r)(c)))

  def constantResultSetConnector[A](c: => Connector[A]): ResultSetConnector[A] =
    resultSetConnector(_ => c)

  def rResultSetConnector[A](f: ResultSet => A): ResultSetConnector[A] =
    resultSetConnector(f(_).η[Connector])

  def resultSetConnection[A](f: ResultSet => Connection => SQLValue[A]): ResultSetConnector[A] =
    resultSetConnector(r => connector(f(r)))

  // WARNING: side-effects on rs
  val next = resultSetConnector((rs: ResultSet) =>
    rs.next.η[Connector])

  def nullType(typ: SQLType) = JDBCType.nullType(typ)
  def booleanType(value: Boolean) = JDBCType.booleanType(value)
  def byteType(value: Byte) = JDBCType.byteType(value)
  def shortType(value: Short) = JDBCType.shortType(value)
  def intType(value: Int) = JDBCType.intType(value)
  def longType(value: Long) = JDBCType.longType(value)
  def floatType(value: Float) = JDBCType.floatType(value)
  def doubleType(value: Double) = JDBCType.doubleType(value)
  def bigDecimalType(value: java.math.BigDecimal) = JDBCType.bigDecimalType(value)
  def stringType(value: String) = JDBCType.stringType(value)
  def bytesType(value: Array[Byte]) = JDBCType.bytesType(value)
  def dateType(value: Date) = JDBCType.dateType(value)
  def timeType(value: Time) = JDBCType.timeType(value)
  def timestampType(value: Timestamp) = JDBCType.timestampType(value)
  def asciiStreamType(value: InputStream, length: Int) = JDBCType.asciiStreamType(value, length)
  def unicodeStreamType(value: InputStream, length: Int) = JDBCType.unicodeStreamType(value, length)
  def binaryStreamType(value: InputStream, length: Int) = JDBCType.binaryStreamType(value, length)
  def objectTypeType(value: AnyRef, typ: SQLType) = JDBCType.objectTypeType(value, typ)
  def objectType(value: AnyRef) = JDBCType.objectType(value)
  def characterStreamType(value: Reader, length: Int) = JDBCType.characterStreamType(value, length)
  def refType(value: Ref) = JDBCType.refType(value)
  def blobType(value: Blob) = JDBCType.blobType(value)
  def clobType(value: Clob) = JDBCType.clobType(value)
  def arrayType(value: java.sql.Array) = JDBCType.arrayType(value)
  def calendarDateType(value: Date, cal: Calendar) = JDBCType.calendarDateType(value, cal)
  def calendarTimeType(value: Time, cal: Calendar) = JDBCType.calendarTimeType(value, cal)
  def calendarTimestampType(value: Timestamp, cal: Calendar) = JDBCType.calendarTimestampType(value, cal)
  def userNullType(typ: SQLType, name: String) = JDBCType.userNullType(typ, name)
  def urlType(value: URL) = JDBCType.urlType(value)

  val sqlTypes = Set(
                      ArrayType
                    , BigIntType
                    , BinaryType
                    , BitType
                    , BlobType
                    , BooleanType
                    , CharType
                    , ClobType
                    , DataLinkType
                    , DateType
                    , DecimalType
                    , DistinctType
                    , DoubleType
                    , FloatType
                    , IntegerType
                    , JavaObjectType
                    , LongNVarCharType
                    , LongVarBinaryType
                    , LongVarCharType
                    , NCharType
                    , NClobType
                    , NullType
                    , NumericType
                    , NVarCharType
                    , OtherType
                    , RealType
                    , RefType
                    , RowIdType
                    , SmallIntType
                    , SqlXmlType
                    , StructType
                    , TimeType
                    , TimestampType
                    , TinyIntType
                    , VarBinaryType
                    , VarCharType
                    )
}