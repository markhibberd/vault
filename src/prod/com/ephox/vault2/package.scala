package com.ephox

import scalaz._
import Scalaz._
import vault2._
import java.io.{Reader, InputStream}
import java.net.URL
import java.util.Calendar
import java.sql.{SQLXML, RowId, Date, Clob, Blob, Ref, Timestamp, Time, PreparedStatement, ResultSet, SQLException, Connection}

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

  def tryRowAccessValue[A](a: => A): RowAccess[A] =
    tryValue(a).toRowAccess

  // alias for η specialised to RowAccess
  def rowAccessValue[A](a: A): RowAccess[A] =
    RowAccess.value(a)

  def rowAccessErr[A](e: SQLException): RowAccess[A] =
    sqlErr(e).toRowAccess

  def rowAccessNull[A]: RowAccess[A] =
    RowAccess.nul[A]

  def rowAccessor[A](f: Row => RowAccess[A]): RowAccessor[A] =
    RowAccessor.rowAccessor(f)

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

    def arrayIndex(columnIndex: Int): RowAccessor[java.sql.Array] = rowAccessor(_.arrayIndex(columnIndex))
    def arrayLabel(columnLabel: String): RowAccessor[java.sql.Array] = rowAccessor(_.arrayLabel(columnLabel))

    def asciiStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.asciiStreamIndex(columnIndex, withInputStream))
    def asciiStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.asciiStreamLabel(columnLabel, withInputStream))

    def bigDecimalIndex(columnIndex: Int): RowAccessor[java.math.BigDecimal] = rowAccessor(_.bigDecimalIndex(columnIndex))
    def bigDecimalLabel(columnLabel: String): RowAccessor[java.math.BigDecimal] = rowAccessor(_.bigDecimalLabel(columnLabel))

    def binaryStreamIndex[A](columnIndex: Int, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.binaryStreamIndex(columnIndex, withInputStream))
    def binaryStreamLabel[A](columnLabel: String, withInputStream: InputStream => A): RowAccessor[A] = rowAccessor(_.binaryStreamLabel(columnLabel, withInputStream))

    def blobIndex(columnIndex: Int): RowAccessor[Blob] = rowAccessor(_.blobIndex(columnIndex))
    def blobLabel(columnLabel: String): RowAccessor[Blob] = rowAccessor(_.blobLabel(columnLabel))

    def booleanIndex(columnIndex: Int): RowAccessor[Boolean] = rowAccessor(_.booleanIndex(columnIndex))
    def booleanLabel(columnLabel: String): RowAccessor[Boolean] = rowAccessor(_.booleanLabel(columnLabel))

    def byteIndex(columnIndex: Int): RowAccessor[Byte] = rowAccessor(_.byteIndex(columnIndex))
    def byteLabel(columnLabel: String): RowAccessor[Byte] = rowAccessor(_.byteLabel(columnLabel))

    def bytesIndex(columnIndex: Int): RowAccessor[Array[Byte]] = rowAccessor(_.bytesIndex(columnIndex))
    def bytesLabel(columnLabel: String): RowAccessor[Array[Byte]] = rowAccessor(_.bytesLabel(columnLabel))

    def characterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.characterStreamIndex(columnIndex, withReader))
    def characterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.characterStreamLabel(columnLabel, withReader))

    def clobIndex(columnIndex: Int): RowAccessor[Clob] = rowAccessor(_.clobIndex(columnIndex))
    def clobLabel(columnLabel: String): RowAccessor[Clob] = rowAccessor(_.clobLabel(columnLabel))

    def dateIndex(columnIndex: Int): RowAccessor[Date] = rowAccessor(_.dateIndex(columnIndex))
    def dateLabel(columnLabel: String): RowAccessor[Date] = rowAccessor(_.dateLabel(columnLabel))
    def dateIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Date] = rowAccessor(_.dateIndexCal(columnIndex, cal))
    def dateLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Date] = rowAccessor(_.dateLabelCal(columnLabel, cal))

    def doubleIndex(columnIndex: Int): RowAccessor[Double] = rowAccessor(_.doubleIndex(columnIndex))
    def doubleLabel(columnLabel: String): RowAccessor[Double] = rowAccessor(_.doubleLabel(columnLabel))

    def floatIndex(columnIndex: Int): RowAccessor[Float] = rowAccessor(_.floatIndex(columnIndex))
    def floatLabel(columnLabel: String): RowAccessor[Float] = rowAccessor(_.floatLabel(columnLabel))

    def intIndex(columnIndex: Int): RowAccessor[Int] = rowAccessor(_.intIndex(columnIndex))
    def intLabel(columnLabel: String): RowAccessor[Int] = rowAccessor(_.intLabel(columnLabel))

    def longIndex(columnIndex: Int): RowAccessor[Long] = rowAccessor(_.longIndex(columnIndex))
    def longLabel(columnLabel: String): RowAccessor[Long] = rowAccessor(_.longLabel(columnLabel))

    def ncharacterStreamIndex[A](columnIndex: Int, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.ncharacterStreamIndex(columnIndex, withReader))
    def ncharacterStreamLabel[A](columnLabel: String, withReader: Reader => A): RowAccessor[A] = rowAccessor(_.ncharacterStreamLabel(columnLabel, withReader))

    def nclobIndex(columnIndex: Int): RowAccessor[Clob] = rowAccessor(_.nclobIndex(columnIndex))
    def nclobLabel(columnLabel: String): RowAccessor[Clob] = rowAccessor(_.nclobLabel(columnLabel))

    def nstringIndex(columnIndex: Int): RowAccessor[String] = rowAccessor(_.nstringIndex(columnIndex))
    def nstringLabel(columnLabel: String): RowAccessor[String] = rowAccessor(_.nstringLabel(columnLabel))

    def objectIndex(columnIndex: Int): RowAccessor[AnyRef] = rowAccessor(_.objectIndex(columnIndex))
    def objectLabel(columnLabel: String): RowAccessor[AnyRef] = rowAccessor(_.objectLabel(columnLabel))
    def objectMapIndex(columnIndex: Int, m: Row.ObjectTypeMap): RowAccessor[AnyRef] = rowAccessor(_.objectMapIndex(columnIndex, m))
    def objectMapLabel(columnLabel: String, m: Row.ObjectTypeMap): RowAccessor[AnyRef] = rowAccessor(_.objectMapLabel(columnLabel, m))

    def refIndex(columnIndex: Int): RowAccessor[Ref] = rowAccessor(_.refIndex(columnIndex))
    def refLabel(columnLabel: String): RowAccessor[Ref] = rowAccessor(_.refLabel(columnLabel))

    def rowIdIndex(columnIndex: Int): RowAccessor[RowId] = rowAccessor(_.rowIdIndex(columnIndex))
    def rowIdLabel(columnLabel: String): RowAccessor[RowId] = rowAccessor(_.rowIdLabel(columnLabel))

    def shortIndex(columnIndex: Int): RowAccessor[Short] = rowAccessor(_.shortIndex(columnIndex))
    def shortLabel(columnLabel: String): RowAccessor[Short] = rowAccessor(_.shortLabel(columnLabel))

    def sqlxmlIndex(columnIndex: Int): RowAccessor[SQLXML] = rowAccessor(_.sqlxmlIndex(columnIndex))
    def sqlxmlLabel(columnLabel: String): RowAccessor[SQLXML] = rowAccessor(_.sqlxmlLabel(columnLabel))

    def stringIndex(columnIndex: Int): RowAccessor[String] = rowAccessor(_.stringIndex(columnIndex))
    def stringLabel(columnLabel: String): RowAccessor[String] = rowAccessor(_.stringLabel(columnLabel))

    def timeIndex(columnIndex: Int): RowAccessor[Time] = rowAccessor(_.timeIndex(columnIndex))
    def timeLabel(columnLabel: String): RowAccessor[Time] = rowAccessor(_.timeLabel(columnLabel))
    def timeIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Time] = rowAccessor(_.timeIndexCal(columnIndex, cal))
    def timeLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Time] = rowAccessor(_.timeLabelCal(columnLabel, cal))

    def timestampIndex(columnIndex: Int): RowAccessor[Timestamp] = rowAccessor(_.timestampIndex(columnIndex))
    def timestampLabel(columnLabel: String): RowAccessor[Timestamp] = rowAccessor(_.timestampLabel(columnLabel))
    def timestampIndexCal(columnIndex: Int, cal: Row.Cal): RowAccessor[Timestamp] = rowAccessor(_.timestampIndexCal(columnIndex, cal))
    def timestampLabelCal(columnLabel: String, cal: Row.Cal): RowAccessor[Timestamp] = rowAccessor(_.timestampLabelCal(columnLabel, cal))

    def urlIndex(columnIndex: Int): RowAccessor[URL] = rowAccessor(_.urlIndex(columnIndex))
    def urlLabel(columnLabel: String): RowAccessor[URL] = rowAccessor(_.urlLabel(columnLabel))
}