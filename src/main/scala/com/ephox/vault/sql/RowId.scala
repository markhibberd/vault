package com.ephox
package vault
package sql

sealed trait RowId {
  private[sql] val x: java.sql.RowId
}

object RowId {
  def apply(xx: java.sql.RowId): RowId =
    new RowId {
      val x = xx
    }
}
