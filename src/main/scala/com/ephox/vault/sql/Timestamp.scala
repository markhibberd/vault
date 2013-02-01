package com.ephox
package vault
package sql

sealed trait Timestamp {
  private[sql] val x: java.sql.Timestamp
}

object Timestamp {
  def apply(xx: java.sql.Timestamp): Timestamp =
    new Timestamp {
      val x = xx
    }
}
