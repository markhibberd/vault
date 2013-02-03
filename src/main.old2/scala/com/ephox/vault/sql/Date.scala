package com.ephox
package vault
package sql

sealed trait Date {
  private[sql] val x: java.sql.Date
}

object Date {
  def apply(xx: java.sql.Date): Date =
    new Date {
      val x = xx
    }
}
