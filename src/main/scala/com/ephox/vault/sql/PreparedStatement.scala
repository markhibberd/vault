package com.ephox
package vault
package sql

sealed trait PreparedStatement {
  private[sql] val x: java.sql.PreparedStatement

  def statement: Statement =
    Statement(x)
}

object PreparedStatement {
  def apply(xx: java.sql.PreparedStatement): PreparedStatement =
    new PreparedStatement {
      val x = xx
    }
}
