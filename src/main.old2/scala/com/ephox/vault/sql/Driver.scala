package com.ephox
package vault
package sql

sealed trait Driver {
  private[sql] val x: java.sql.Driver
}

object Driver {
  def apply(xx: java.sql.Driver): Driver =
    new Driver {
      val x = xx
    }
}
