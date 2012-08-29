package com.ephox
package vault
package sql

sealed trait Statement {
  private[sql] val x: java.sql.Statement
}

object Statement {
  def apply(xx: java.sql.Statement): Statement =
    new Statement {
      val x = xx
    }
}
