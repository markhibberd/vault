package com.ephox
package vault
package sql

sealed trait Connection {
  private[sql] val x: java.sql.Connection
}

object Connection {
  def apply(xx: java.sql.Connection): Connection =
    new Connection {
      val x = xx
    }
}
