package com.ephox
package vault
package sql

sealed trait DatabaseMetaData {
  private[sql] val x: java.sql.DatabaseMetaData
}

object DatabaseMetaData {
  def apply(xx: java.sql.DatabaseMetaData): DatabaseMetaData =
    new DatabaseMetaData {
      val x = xx
    }
}
