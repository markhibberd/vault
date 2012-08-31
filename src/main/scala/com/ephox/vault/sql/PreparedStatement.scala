package com.ephox
package vault
package sql

import SqlT._

sealed trait PreparedStatement {
  private[sql] val x: java.sql.PreparedStatement

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

}

object PreparedStatement {
  def apply(xx: java.sql.PreparedStatement): PreparedStatement =
    new PreparedStatement {
      val x = xx
    }
}
