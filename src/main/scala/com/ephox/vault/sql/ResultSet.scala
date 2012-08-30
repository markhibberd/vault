package com.ephox
package vault
package sql

sealed trait ResultSet {
  private[sql] val x: java.sql.ResultSet
}

object ResultSet {
  def apply(xx: java.sql.ResultSet): ResultSet =
    new ResultSet {
      val x = xx
    }
}
