package com.ephox
package vault
package sql

sealed trait Array {
  private[sql] val x: java.sql.Array
}

object Array {
  def apply(xx: java.sql.Array): Array =
    new Array {
      val x = xx
    }
}
