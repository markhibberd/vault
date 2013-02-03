package com.ephox
package vault
package sql

sealed trait Savepoint {
  private[sql] val x: java.sql.Savepoint
}

object Savepoint {
  def apply(xx: java.sql.Savepoint): Savepoint =
    new Savepoint {
      val x = xx
    }
}
