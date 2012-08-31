package com.ephox
package vault
package sql

sealed trait ResultSetMetaData {
  private[sql] val x: java.sql.ResultSetMetaData
}

object ResultSetMetaData {
  def apply(xx: java.sql.ResultSetMetaData): ResultSetMetaData =
    new ResultSetMetaData {
      val x = xx
    }
}
