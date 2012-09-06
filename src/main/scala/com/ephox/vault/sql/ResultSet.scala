package com.ephox
package vault
package sql

import java.sql.{ResultSet => R}
import scalaz._, Scalaz._
import collection.JavaConversions._
import SqlT._
import XSqlT._

sealed trait ResultSet {
  private[sql] val x: java.sql.ResultSet

  def absolute(row: Int): Sql[Boolean] =
    Try(x.absolute(row))

  def afterLast: Sql[Unit] =
    Try(x.afterLast)

  def beforeFirst: Sql[Unit] =
    Try(x.beforeFirst)

  def cancelRowUpdates: Sql[Unit] =
    Try(x.cancelRowUpdates)

  def clearWarnings: Sql[Unit] =
    Try(x.clearWarnings)

  def close: Sql[Unit] =
    Try(x.close)

  def deleteRow: Sql[Unit] =
    Try(x.deleteRow)

  def findColumn(columnName: String): Sql[Int] =
    Try(x.findColumn(columnName))

  def first: Sql[Boolean] =
    Try(x.first)

  def array(q: Column): XGetSet[Array] =
    XGetSet(q, Array(q.fold(x getArray _, x getArray _)), a => q.fold(i => x.updateArray(i, a.x), n => x.updateArray(n, a.x)))

  def asciiStream(q: Column): XGetSet[java.io.InputStream] =
    XGetSet(q, q.fold(x getAsciiStream _, x getAsciiStream _), a => q.fold(i => x.updateAsciiStream(i, a), n => x.updateAsciiStream(n, a)))

  def bigDecimal(q: Column): XGetSet[java.math.BigDecimal] =
    XGetSet(q, q.fold(x getBigDecimal _, x getBigDecimal _), a => q.fold(i => x.updateBigDecimal(i, a), n => x.updateBigDecimal(n, a)))

  def binaryStream(q: Column): XGetSet[java.io.InputStream] =
    XGetSet(q, q.fold(x getBinaryStream _, x getBinaryStream _), a => q.fold(i => x.updateBinaryStream(i, a), n => x.updateBinaryStream(n, a)))

  def blob(q: Column): XGetSet[Blob] =
    XGetSet(q, Blob(q.fold(x getBlob _, x getBlob _)), a => q.fold(i => x.updateBlob(i, a.x), n => x.updateBlob(n, a.x)))

  def boolean(q: Column): GetSet[Boolean] =
    GetSet(q, q.fold(x getBoolean _, x getBoolean _), a => q.fold(i => x.updateBoolean(i, a), n => x.updateBoolean(n, a)))

  def byte(q: Column): GetSet[Byte] =
    GetSet(q, q.fold(x getByte _, x getByte _), a => q.fold(i => x.updateByte(i, a), n => x.updateByte(n, a)))

  def bytes(q: Column): XGetSet[scala.Array[Byte]] =
    XGetSet(q, q.fold(x getBytes _, x getBytes _), a => q.fold(i => x.updateBytes(i, a), n => x.updateBytes(n, a)))

  def characterStream(q: Column): XGetSet[java.io.Reader] =
    XGetSet(q, q.fold(x getCharacterStream _, x getCharacterStream _), a => q.fold(i => x.updateCharacterStream(i, a), n => x.updateCharacterStream(n, a)))

  def clob(q: Column): XGetSet[Clob] =
    XGetSet(q, Clob(q.fold(x getClob _, x getClob _)), a => q.fold(i => x.updateClob(i, a.x), n => x.updateClob(n, a.x)))

  def concurrency: Sql[ResultSetConcurrency] =
    Try(x.getConcurrency) map (c =>
      if(c == R.CONCUR_READ_ONLY)
        ResultSetConcurrency.ReadOnly
      else if(c == R.CONCUR_UPDATABLE)
        ResultSetConcurrency.Updatable
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSet.html#getConcurrency%28%29""")
      )

  def cursorName: Sql[String] =
    Try(x.getCursorName)

  def date(q: Column, m: Option[java.util.Calendar]): XGetSet[Date] =
    XGetSet(q, Date(q.fold(
      i => m match {
        case None => x.getDate(i)
        case Some(r) => x.getDate(i, r)
      }
    , n => m match {
      case None => x.getDate(n)
      case Some(r) => x.getDate(n, r)
    })
    ), t => q.fold(
      i => x.updateDate(i, t.x)
    , n => x.updateDate(n, t.x)
    ))

  def double(q: Column): GetSet[Double] =
    GetSet(q, q.fold(x getDouble _, x getDouble _), a => q.fold(i => x.updateDouble(i, a), n => x.updateDouble(n, a)))

  def fetchDirection: Sql[FetchDirection] =
    Try(x.getFetchDirection) map (c =>
      if(c == R.FETCH_FORWARD)
        FetchDirection.Forward
      else if(c == R.FETCH_REVERSE)
        FetchDirection.Reverse
      else if(c == R.FETCH_UNKNOWN)
        FetchDirection.Unknown
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSet.html#setFetchDirection%28int%29""")
      )

  def fetchSize: Sql[Int] =
    Try(x.getFetchSize)

  def float(q: Column): GetSet[Float] =
    GetSet(q, q.fold(x getFloat _, x getFloat _), a => q.fold(i => x.updateFloat(i, a), n => x.updateFloat(n, a)))

  def int(q: Column): GetSet[Int] =
    GetSet(q, q.fold(x getInt _, x getInt _), a => q.fold(i => x.updateInt(i, a), n => x.updateInt(n, a)))

  def long(q: Column): GetSet[Long] =
    GetSet(q, q.fold(x getLong _, x getLong _), a => q.fold(i => x.updateLong(i, a), n => x.updateLong(n, a)))

  def metaData: Sql[ResultSetMetaData] =
    Try(ResultSetMetaData(x.getMetaData))

  def obj(q: Column, m: Option[collection.mutable.Map[String, Class[_]]]): XGetSet[AnyRef] =
    XGetSet(q, q.fold(
      i => m match {
        case None => x.getObject(i)
        case Some(r) => x.getObject(i, r)
      }
    , n => m match {
      case None => x.getObject(n)
      case Some(r) => x.getObject(n, r)
    })
    , t => q.fold(
      i => x.updateObject(i, t)
    , n => x.updateObject(n, t)
    ))

  def ref(q: Column): XGetSet[Ref] =
    XGetSet(q, Ref(q.fold(x getRef _, x getRef _)), a => q.fold(i => x.updateRef(i, a.x), n => x.updateRef(n, a.x)))

  def short(q: Column): GetSet[Short] =
    GetSet(q, q.fold(x getShort _, x getShort _), a => q.fold(i => x.updateShort(i, a), n => x.updateShort(n, a)))

  def statement: Sql[Statement] =
    Try(Statement(x.getStatement))

  def string(q: Column): XGetSet[String] =
    XGetSet(q, q.fold(x getString _, x getString _), a => q.fold(i => x.updateString(i, a), n => x.updateString(n, a)))

  def time(q: Column, m: Option[java.util.Calendar]): XGetSet[Time] =
    XGetSet(q, Time(q.fold(
      i => m match {
        case None => x.getTime(i)
        case Some(r) => x.getTime(i, r)
      }
    , n => m match {
      case None => x.getTime(n)
      case Some(r) => x.getTime(n, r)
    })
    ), t => q.fold(
      i => x.updateTime(i, t.x)
    , n => x.updateTime(n, t.x)
    ))

  def timestamp(q: Column, m: Option[java.util.Calendar]): XGetSet[Timestamp] =
    XGetSet(q, Timestamp(q.fold(
      i => m match {
        case None => x.getTimestamp(i)
        case Some(r) => x.getTimestamp(i, r)
      }
    , n => m match {
      case None => x.getTimestamp(n)
      case Some(r) => x.getTimestamp(n, r)
    })
    ), t => q.fold(
      i => x.updateTimestamp(i, t.x)
    , n => x.updateTimestamp(n, t.x)
    ))

  def ty: Sql[ResultSetType] =
    Try(x.getType) map (c =>
      if(c == R.TYPE_FORWARD_ONLY)
        ResultSetType.ForwardOnly
      else if(c == R.TYPE_SCROLL_INSENSITIVE)
        ResultSetType.ScrollInsensitive
      else if(c == R.TYPE_SCROLL_SENSITIVE)
        ResultSetType.ScrollSensitive
      else
        sys.error("[" + c + """] http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/ResultSet.html#getType%28%29""")
      )

  def url(q: Column): XSql[java.net.URL] =
    XTry(q.fold(
      x.getURL(_)
    , x.getURL(_)
    ))

  def warnings: XSql[java.sql.SQLWarning] =
    XTry(x.getWarnings)

  def insertRow: Sql[Unit] =
    Try(x.insertRow)

  def isAfterLast: Sql[Boolean] =
    Try(x.isAfterLast)

  def isBeforeFirst: Sql[Boolean] =
    Try(x.isBeforeFirst)

  def isFirst: Sql[Boolean] =
    Try(x.isFirst)

  def isLast: Sql[Boolean] =
    Try(x.isLast)

  def last: Sql[Boolean] =
    Try(x.last)

  def moveToCurrentRow: Sql[Unit] =
    Try(x.moveToCurrentRow)

  def moveToInsertRow: Sql[Unit] =
    Try(x.moveToInsertRow)

  def next: Sql[Boolean] =
    Try(x.next)

  def previous: Sql[Boolean] =
    Try(x.previous)

  def refreshRow: Sql[Unit] =
    Try(x.refreshRow)

  def relative(rows: Int): Sql[Boolean] =
    Try(x.relative(rows))

  def rowDeleted: Sql[Boolean] =
    Try(x.rowDeleted)

  def rowInserted: Sql[Boolean] =
    Try(x.rowInserted)

  def rowUpdated: Sql[Boolean] =
    Try(x.rowUpdated)

  def setFetchDirection(direction: FetchDirection): Sql[Unit] =
    Try(x.setFetchDirection(direction.int))

  def setFetchSize(rows: Int): Sql[Unit] =
    Try(x.setFetchSize(rows))

  def wasNull: Sql[Boolean] =
    Try(x.wasNull)

}

object ResultSet {
  def apply(xx: java.sql.ResultSet): ResultSet =
    new ResultSet {
      val x = xx
    }
}



