package com.ephox
package vault
package sql

import SqlT._
import collection.JavaConversions._
import scalaz._, Scalaz._

sealed trait CallableStatement {
  private[sql] val x: java.sql.CallableStatement

  def preparedStatement: PreparedStatement =
    PreparedStatement(x)

  def statement: Statement =
    preparedStatement.statement

  def array(q: Int \/ String): Sql[Array] =
    Try(Array(q.fold(
      x.getArray(_)
    , x.getArray(_)
    )))

  def bigDecimal(q: Int \/ String): Sql[BigDecimal] =
    Try(q.fold(
      x.getBigDecimal(_)
    , x.getBigDecimal(_)
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

  def clob(q: Int \/ String): Sql[Clob] =
    Try(Clob(q.fold(
      x.getClob(_)
    , x.getClob(_)
    )))

  def date(q: Int \/ String): Sql[Date] =
    Try(Date(q.fold(
      x.getDate(_)
    , x.getDate(_)
    )))

  def double(q: Int \/ String): Sql[Double] =
    Try(q.fold(
      x.getDouble(_)
    , x.getDouble(_)
    ))

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

  def url(q: Int \/ String): Sql[java.net.URL] =
    Try(q.fold(
      x.getURL(_)
    , x.getURL(_)
    ))

}

object CallableStatement {
  def apply(xx: java.sql.CallableStatement): CallableStatement =
    new CallableStatement {
      val x = xx
    }
}
