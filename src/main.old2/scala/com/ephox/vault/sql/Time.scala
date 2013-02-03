package com.ephox
package vault
package sql

sealed trait Time {
  private[sql] val x: java.sql.Time
}

object Time {
  def apply(xx: java.sql.Time): Time =
    new Time {
      val x = xx
    }
}
