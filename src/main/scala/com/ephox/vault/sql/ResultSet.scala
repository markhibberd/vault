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

  def array(q: Int \/ String): XSql[Array] =
    TryNull(q.fold(
      x.getArray(_)
    , x.getArray(_)
    )) map (Array(_))

  def asciiStream(q: Int \/ String): XSql[java.io.InputStream] =
    TryNull(q.fold(
      x.getAsciiStream(_)
    , x.getAsciiStream(_)
    ))

  def bigDecimal(q: Int \/ String): XSql[BigDecimal] =
    TryNull(q.fold(
      x.getBigDecimal(_)
    , x.getBigDecimal(_)
    ))

  def binaryStream(q: Int \/ String): XSql[java.io.InputStream] =
    TryNull(q.fold(
      x.getBinaryStream(_)
    , x.getBinaryStream(_)
    ))

  def blob(q: Int \/ String): XSql[Blob] =
    TryNull(q.fold(
      x.getBlob(_)
    , x.getBlob(_)
    )) map (Blob(_))

  def boolean(q: Column): GetSet[Boolean] =
    GetSet(q, q.fold(x getBoolean _, x getBoolean _), a => q.fold(i => x.updateBoolean(i, a), n => x.updateBoolean(n, a)))

  def byte(q: Int \/ String): Sql[Byte] =
    Try(q.fold(
      x.getByte(_)
    , x.getByte(_)
    ))

  def bytes(q: Column): GetSet[scala.Array[Byte]] =
    GetSet(q, q.fold(x getBytes _, x getBytes _), a => q.fold(i => x.updateBytes(i, a), n => x.updateBytes(n, a)))

  def characterStream(q: Int \/ String): XSql[java.io.Reader] =
    TryNull(q.fold(
      x.getCharacterStream(_)
    , x.getCharacterStream(_)
    ))

  def clob(q: Int \/ String): XSql[Clob] =
    TryNull(q.fold(
      x.getClob(_)
    , x.getClob(_)
    )) map (Clob(_))

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

  def date(q: Int \/ String, m: Option[java.util.Calendar]): XSql[Date] =
    TryNull(q.fold(
      i => m match {
        case None => x.getDate(i)
        case Some(r) => x.getDate(i, r)
      }
      , n => m match {
        case None => x.getDate(n)
        case Some(r) => x.getDate(n, r)
      }
    )) map (Date(_))

  def double(q: Int \/ String): Sql[Double] =
    Try(q.fold(
      x.getDouble(_)
    , x.getDouble(_)
    ))

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

  def float(q: Int \/ String): Sql[Float] =
    Try(q.fold(
      x.getFloat(_)
    , x.getFloat(_)
    ))

  def int(q: Int \/ String): Sql[Int] =
    Try(q.fold(
      x.getInt(_)
    , x.getInt(_)
    ))

  def long(q: Int \/ String): Sql[Long] =
    Try(q.fold(
      x.getLong(_)
    , x.getLong(_)
    ))

  def metaData: Sql[ResultSetMetaData] =
    Try(ResultSetMetaData(x.getMetaData))

  def obj(q: Int \/ String, m: Option[collection.mutable.Map[String, Class[_]]]): XSql[AnyRef] =
    TryNull(q.fold(
      i => m match {
        case None => x.getObject(i)
        case Some(r) => x.getObject(i, r)
      }
      , n => m match {
        case None => x.getObject(n)
        case Some(r) => x.getObject(n, r)
      }
    ))

  def ref(q: Int \/ String): XSql[Ref] =
    TryNull(q.fold(
      x.getRef(_)
    , x.getRef(_)
    )) map (Ref(_))

  def short(q: Int \/ String): Sql[Short] =
    Try(q.fold(
      x.getShort(_)
    , x.getShort(_)
    ))

  def statement: Sql[Statement] =
    Try(Statement(x.getStatement))

  def string(q: Int \/ String): XSql[String] =
    TryNull(q.fold(
      x.getString(_)
    , x.getString(_)
    ))

  def time(q: Int \/ String, m: Option[java.util.Calendar]): XSql[Time] =
    TryNull(q.fold(
      i => m match {
        case None => x.getTime(i)
        case Some(r) => x.getTime(i, r)
      }
      , n => m match {
        case None => x.getTime(n)
        case Some(r) => x.getTime(n, r)
      }
    )) map (Time(_))

  def timestamp(q: Int \/ String, m: Option[java.util.Calendar]): XSql[Timestamp] =
    TryNull(q.fold(
      i => m match {
        case None => x.getTimestamp(i)
        case Some(r) => x.getTimestamp(i, r)
      }
      , n => m match {
        case None => x.getTimestamp(n)
        case Some(r) => x.getTimestamp(n, r)
      }
    )) map (Timestamp(_))

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

  def url(q: Int \/ String): XSql[java.net.URL] =
    TryNull(q.fold(
      x.getURL(_)
    , x.getURL(_)
    ))

  def warnings: XSql[java.sql.SQLWarning] =
    TryNull(x.getWarnings)

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

}

object ResultSet {
  def apply(xx: java.sql.ResultSet): ResultSet =
    new ResultSet {
      val x = xx
    }
}



