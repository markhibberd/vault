package com.ephox
package vault
package sql

import SqlT._

sealed trait PreparedStatement {
  private[sql] val x: java.sql.PreparedStatement

  import PreparedStatement._

  def statement: Statement =
    Statement(x)

  def addBatch: Sql[Unit] =
    Try(x.addBatch)

  def clearParameters: Sql[Unit] =
    Try(x.clearParameters)

  def execute: Sql[Boolean] =
    Try(x.execute)

  def executeQuery: Sql[ResultSet] =
    Try(ResultSet(x.executeQuery))

  def executeUpdate: Sql[Int] =
    Try(x.executeUpdate)

  def metaData: Sql[ResultSetMetaData] =
    Try(ResultSetMetaData(x.getMetaData))

  def parameterMetaData: Sql[ParameterMetaData] =
    Try(ParameterMetaData(x.getParameterMetaData))

  def setArray(i: Int, a: Array): Sql[Unit] =
    Try(x.setArray(i, a.x))

  def setAsciiStream(i: Int, o: java.io.InputStream, length: Int): Sql[Unit] =
    Try(x.setAsciiStream(i, o, length))

  def setBigDecimal(i: Int, a: java.math.BigDecimal): Sql[Unit] =
    Try(x.setBigDecimal(i, a))

  def setBinaryStream(i: Int, a: java.io.InputStream, length: Int): Sql[Unit] =
    Try(x.setBinaryStream(i, a, length))

  def setBlob(i: Int, a: Blob): Sql[Unit] =
    Try(x.setBlob(i, a.x))

  def setBoolean(i: Int, a: Boolean): Sql[Unit] =
    Try(x.setBoolean(i, a))

  def setByte(i: Int, a: Byte): Sql[Unit] =
    Try(x.setByte(i, a))

  def setBytes(i: Int, a: scala.Array[Byte]): Sql[Unit] =
    Try(x.setBytes(i, a))

  def setCharacterStream(i: Int, a: java.io.Reader, length: Int): Sql[Unit] =
    Try(x.setCharacterStream(i, a, length))

  def setClob(i: Int, a: Clob): Sql[Unit] =
    Try(x.setClob(i, a.x))

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

  def setRef(i: Int, a: Ref): Sql[Unit] =
    Try(x.setRef(i, a.x))

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

}

object PreparedStatement {
  def apply(xx: java.sql.PreparedStatement): PreparedStatement =
    new PreparedStatement {
      val x = xx
    }

  sealed trait SetObject
  object SetObject {
    case object None extends SetObject
    case class TargetType(t: SqlType) extends SetObject
    case class TargetTypeScale(t: SqlType, s: Int) extends SetObject

  }
}
