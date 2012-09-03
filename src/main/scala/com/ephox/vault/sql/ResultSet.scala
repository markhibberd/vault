package com.ephox
package vault
package sql

import java.sql.{ResultSet => R}
import scalaz._, Scalaz._
import collection.JavaConversions._
import SqlT._

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

  def array(q: Int \/ String): Sql[Array] =
    Try(Array(q.fold(
      x.getArray(_)
    , x.getArray(_)
    )))

  def asciiStream(q: Int \/ String): Sql[java.io.InputStream] =
    Try(q.fold(
      x.getAsciiStream(_)
    , x.getAsciiStream(_)
    ))

  def bigDecimal(q: Int \/ String): Sql[BigDecimal] =
    Try(q.fold(
      x.getBigDecimal(_)
    , x.getBigDecimal(_)
    ))

  def binaryStream(q: Int \/ String): Sql[java.io.InputStream] =
    Try(q.fold(
      x.getBinaryStream(_)
    , x.getBinaryStream(_)
    ))

  def blob(q: Int \/ String): Sql[Blob] =
    Try(Blob(q.fold(
      x.getBlob(_)
    , x.getBlob(_)
    )))

  def boolean(q: Int \/ String): Sql[Boolean] =
    Try(q.fold(
      x.getBoolean(_)
    , x.getBoolean(_)
    ))

  def byte(q: Int \/ String): Sql[Byte] =
    Try(q.fold(
      x.getByte(_)
    , x.getByte(_)
    ))

  def bytes(q: Int \/ String): Sql[scala.Array[Byte]] =
    Try(q.fold(
      x.getBytes(_)
    , x.getBytes(_)
    ))

  def characterStream(q: Int \/ String): Sql[java.io.Reader] =
    Try(q.fold(
      x.getCharacterStream(_)
    , x.getCharacterStream(_)
    ))

  def clob(q: Int \/ String): Sql[Clob] =
    Try(Clob(q.fold(
      x.getClob(_)
    , x.getClob(_)
    )))

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

  def date(q: Int \/ String, m: Option[java.util.Calendar]): Sql[Date] =
    Try(Date(q.fold(
      i => m match {
        case None => x.getDate(i)
        case Some(r) => x.getDate(i, r)
      }
      , n => m match {
        case None => x.getDate(n)
        case Some(r) => x.getDate(n, r)
      }
    )))

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

  def obj(q: Int \/ String, m: Option[collection.mutable.Map[String, Class[_]]]): Sql[AnyRef] =
    Try(q.fold(
      i => m match {
        case None => x.getObject(i)
        case Some(r) => x.getObject(i, r)
      }
      , n => m match {
        case None => x.getObject(n)
        case Some(r) => x.getObject(n, r)
      }
    ))

  def ref(q: Int \/ String): Sql[Ref] =
    Try(Ref(q.fold(
      x.getRef(_)
    , x.getRef(_)
    )))

  def short(q: Int \/ String): Sql[Short] =
    Try(q.fold(
      x.getShort(_)
    , x.getShort(_)
    ))

  def statement: Sql[Statement] =
    Try(Statement(x.getStatement))

  def string(q: Int \/ String): Sql[String] =
    Try(q.fold(
      x.getString(_)
    , x.getString(_)
    ))

  def time(q: Int \/ String, m: Option[java.util.Calendar]): Sql[Time] =
    Try(Time(q.fold(
      i => m match {
        case None => x.getTime(i)
        case Some(r) => x.getTime(i, r)
      }
      , n => m match {
        case None => x.getTime(n)
        case Some(r) => x.getTime(n, r)
      }
    )))

  def timestamp(q: Int \/ String, m: Option[java.util.Calendar]): Sql[Timestamp] =
    Try(Timestamp(q.fold(
      i => m match {
        case None => x.getTimestamp(i)
        case Some(r) => x.getTimestamp(i, r)
      }
      , n => m match {
        case None => x.getTimestamp(n)
        case Some(r) => x.getTimestamp(n, r)
      }
    )))

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

  def url(q: Int \/ String): Sql[java.net.URL] =
    Try(q.fold(
      x.getURL(_)
    , x.getURL(_)
    ))

}

object ResultSet {
  def apply(xx: java.sql.ResultSet): ResultSet =
    new ResultSet {
      val x = xx
    }
}



