package com.ephox
package vault
package sql

import SqlT._
import ISqlT._
import XSqlT._

sealed trait ResultSetGet {
  val x: ResultSet

  def findColumn(columnName: String): Sql[Int] =
    x findColumn columnName

  def array(q: Column): XGetSet[Array] =
    x array q

  def asciiStream(q: Column): XGetSet[java.io.InputStream] =
    x asciiStream q

  def bigDecimal(q: Column): XGetSet[java.math.BigDecimal] =
    x bigDecimal q

  def binaryStream(q: Column): XGetSet[java.io.InputStream] =
    x binaryStream q

  def blob(q: Column): XGetSet[Blob] =
    x blob q

  def boolean(q: Column): GetSet[Boolean] =
    x boolean q

  def byte(q: Column): GetSet[Byte] =
    x byte q

  def bytes(q: Column): XGetSet[scala.Array[Byte]] =
    x bytes q

  def characterStream(q: Column): XGetSet[java.io.Reader] =
    x characterStream q

  def clob(q: Column): XGetSet[Clob] =
    x clob q

  def concurrency: ISql[ResultSetConcurrency] =
    x.concurrency

  def cursorName: Sql[String] =
    x.cursorName

  def date(q: Column, m: Option[java.util.Calendar]): XGetSet[Date] =
    x date (q, m)

  def double(q: Column): GetSet[Double] =
    x double q

  def fetchDirection: ISql[FetchDirection] =
    x.fetchDirection

  def fetchSize: Sql[Int] =
    x.fetchSize

  def float(q: Column): GetSet[Float] =
    x float q

  def int(q: Column): GetSet[Int] =
    x int q

  def long(q: Column): GetSet[Long] =
    x long q

  def metaData: Sql[ResultSetMetaData] =
    x.metaData

  def obj(q: Column, m: Option[collection.mutable.Map[String, Class[_]]]): XGetSet[AnyRef] =
    x obj (q, m)

  def ref(q: Column): XGetSet[Ref] =
    x ref q

  def short(q: Column): GetSet[Short] =
    x short q

  def statement: Sql[Statement] =
    x.statement

  def string(q: Column): XGetSet[String] =
    x string q

  def time(q: Column, m: Option[java.util.Calendar]): XGetSet[Time] =
    x time (q, m)

  def timestamp(q: Column, m: Option[java.util.Calendar]): XGetSet[Timestamp] =
    x timestamp (q, m)

  def ty: ISql[ResultSetType] =
    x.ty

  def url(q: Column): XSql[java.net.URL] =
    x url q

  def warnings: XSql[java.sql.SQLWarning] =
    x.warnings

  def rowDeleted: Sql[Boolean] =
    x.rowDeleted

  def rowInserted: Sql[Boolean] =
    x.rowInserted

  def rowUpdated: Sql[Boolean] =
    x.rowUpdated

  def wasNull: Sql[Boolean] =
    x.wasNull

}

object ResultSetGet {
  def apply(xx: ResultSet): ResultSetGet =
    new ResultSetGet {
      val x = xx
    }
}
