package com.ephox
package vault
package sql

import SqlT._
import XSqlT._
import collection.JavaConversions._
import scalaz._, Scalaz._

sealed trait CallableStatement {
  private[sql] val x: java.sql.CallableStatement

  import CallableStatement._

  def preparedStatement: PreparedStatement =
    PreparedStatement(x)

  def statement: Statement =
    preparedStatement.statement

  def array(q: Column): XSql[Array] =
    XTry(q.fold(
      x.getArray(_)
    , x.getArray(_)
    )) map (Array(_))

  def bigDecimal(q: Column): XSql[BigDecimal] =
    XTry(q.fold(
      x.getBigDecimal(_)
    , x.getBigDecimal(_)
    ))

  def blob(q: Column): XSql[Blob] =
    XTry((q.fold(
      x.getBlob(_)
    , x.getBlob(_)
    ))) map (Blob(_))

  def boolean(q: Column): Sql[Boolean] =
    Try(q.fold(
      x.getBoolean(_)
    , x.getBoolean(_)
    ))

  def byte(q: Column): Sql[Byte] =
    Try(q.fold(
      x.getByte(_)
    , x.getByte(_)
    ))

  def bytes(q: Column): XSql[scala.Array[Byte]] =
    XTry(q.fold(
      x.getBytes(_)
    , x.getBytes(_)
    ))

  def clob(q: Column): XSql[Clob] =
    XTry(q.fold(
      x.getClob(_)
    , x.getClob(_)
    )) map (Clob(_))

  def date(q: Column): XSql[Date] =
    XTry(q.fold(
      x.getDate(_)
    , x.getDate(_)
    )) map (Date(_))

  def double(q: Column): Sql[Double] =
    Try(q.fold(
      x.getDouble(_)
    , x.getDouble(_)
    ))

  def float(q: Column): Sql[Float] =
    Try(q.fold(
      x.getFloat(_)
    , x.getFloat(_)
    ))

  def int(q: Column): Sql[Int] =
    Try(q.fold(
      x.getInt(_)
    , x.getInt(_)
    ))

  def long(q: Column): Sql[Long] =
    Try(q.fold(
      x.getLong(_)
    , x.getLong(_)
    ))

  def obj(q: Column, m: Option[collection.mutable.Map[String, Class[_]]]): XSql[AnyRef] =
    XTry(q.fold(
      i => m match {
        case None => x.getObject(i)
        case Some(r) => x.getObject(i, r)
      }
      , n => m match {
        case None => x.getObject(n)
        case Some(r) => x.getObject(n, r)
      }
    ))

  def ref(q: Column): XSql[Ref] =
    XTry(q.fold(
      x.getRef(_)
    , x.getRef(_)
    )) map (Ref(_))

  def short(q: Column): Sql[Short] =
    Try(q.fold(
      x.getShort(_)
    , x.getShort(_)
    ))

  def string(q: Column): XSql[String] =
    XTry(q.fold(
      x.getString(_)
    , x.getString(_)
    ))

  def time(q: Column, m: Option[java.util.Calendar]): XSql[Time] =
    XTry(q.fold(
      i => m match {
        case None => x.getTime(i)
        case Some(r) => x.getTime(i, r)
      }
      , n => m match {
        case None => x.getTime(n)
        case Some(r) => x.getTime(n, r)
      }
    )) map (Time(_))

  def timestamp(q: Column, m: Option[java.util.Calendar]): XSql[Timestamp] =
    XTry(q.fold(
      i => m match {
        case None => x.getTimestamp(i)
        case Some(r) => x.getTimestamp(i, r)
      }
      , n => m match {
        case None => x.getTimestamp(n)
        case Some(r) => x.getTimestamp(n, r)
      }
    )) map (Timestamp(_))

  def url(q: Column): Sql[java.net.URL] =
    Try(q.fold(
      x.getURL(_)
    , x.getURL(_)
    ))

  def registerOutParameter(q: Column, t: SqlType, w: RegisterOutParameter): Sql[Unit] =
    Try(w match {
      case RegisterOutParameter.None =>
        q.fold(
          i => x.registerOutParameter(i, t.int)
        , s => x.registerOutParameter(s, t.int)
        )
      case RegisterOutParameter.Scale(r) =>
        q.fold(
          i => x.registerOutParameter(i, t.int, r)
        , s => x.registerOutParameter(s, t.int, r)
        )
      case RegisterOutParameter.TypeName(n) =>
        q.fold(
          i => x.registerOutParameter(i, t.int, n)
        , s => x.registerOutParameter(s, t.int, n)
        )
    })

  def setAsciiStream(i: Int, o: java.io.InputStream, length: Int): Sql[Unit] =
    Try(x.setAsciiStream(i, o, length))

  def setBigDecimal(i: Int, a: java.math.BigDecimal): Sql[Unit] =
    Try(x.setBigDecimal(i, a))

  def setBinaryStream(i: Int, a: java.io.InputStream, length: Int): Sql[Unit] =
    Try(x.setBinaryStream(i, a, length))

  def setBoolean(i: Int, a: Boolean): Sql[Unit] =
    Try(x.setBoolean(i, a))

  def setByte(i: Int, a: Byte): Sql[Unit] =
    Try(x.setByte(i, a))

  def setBytes(i: Int, a: scala.Array[Byte]): Sql[Unit] =
    Try(x.setBytes(i, a))

  def setCharacterStream(i: Int, a: java.io.Reader, length: Int): Sql[Unit] =
    Try(x.setCharacterStream(i, a, length))

  def setDate(i: Int, a: Date, c: Option[java.util.Calendar]): Sql[Unit] =
    Try(c match {
      case None => x.setDate(i, a.x)
      case Some(d) => x.setDate(i, a.x, d)
    })

  def setDouble(i: Int, a: Double): Sql[Unit] =
    Try(x.setDouble(i, a))

  def setFloat(i: Int, a: Float): Sql[Unit] =
    Try(x.setFloat(i, a))

  def setInt(i: Int, a: Int): Sql[Unit] =
    Try(x.setInt(i, a))

  def setLong(i: Int, a: Long): Sql[Unit] =
    Try(x.setLong(i, a))

  def setNull(i: Int, a: SqlType, name: Option[String]): Sql[Unit] =
    Try(name match {
      case None => x.setNull(i, a.int)
      case Some(d) => x.setNull(i, a.int, d)
    })

  def setObject(i: Int, a: AnyRef, q: SetObject): Sql[Unit] =
    Try(q match {
      case SetObject.None => x.setObject(i, a)
      case SetObject.TargetType(t) => x.setObject(i, a, t.int)
      case SetObject.TargetTypeScale(t, s) => x.setObject(i, a, t.int, s)
    })

  def setShort(i: Int, a: Short): Sql[Unit] =
    Try(x.setShort(i, a))

  def setString(i: Int, a: String): Sql[Unit] =
    Try(x.setString(i, a))

  def setTime(i: Int, a: Time): Sql[Unit] =
    Try(x.setTime(i, a.x))

  def setTimestamp(i: Int, a: Timestamp, c: Option[java.util.Calendar]): Sql[Unit] =
    Try(c match {
      case None => x.setTimestamp(i, a.x)
      case Some(d) => x.setTimestamp(i, a.x, d)
    })

  def setURL(i: Int, a: java.net.URL): Sql[Unit] =
    Try(x.setURL(i, a))

  def wasNull: Sql[Boolean] =
    Try(x.wasNull)
}

object CallableStatement {
  def apply(xx: java.sql.CallableStatement): CallableStatement =
    new CallableStatement {
      val x = xx
    }

  sealed trait RegisterOutParameter
  object RegisterOutParameter {
    case object None extends RegisterOutParameter
    case class Scale(scale: Int) extends RegisterOutParameter
    case class TypeName(name: String) extends RegisterOutParameter

  }
}