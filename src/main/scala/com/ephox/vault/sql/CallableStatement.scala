package com.ephox
package vault
package sql

sealed trait CallableStatement {
  private[sql] val x: java.sql.CallableStatement

  def preparedStatement: PreparedStatement =
    PreparedStatement(x)
}

object CallableStatement {
  def apply(xx: java.sql.CallableStatement): CallableStatement =
    new CallableStatement {
      val x = xx
    }
}
