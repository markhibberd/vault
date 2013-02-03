package com.ephox
package vault
package sql

sealed trait Clob {
  private[sql] val x: java.sql.Clob
}

object Clob {
  def apply(xx: java.sql.Clob): Clob =
    new Clob {
      val x = xx
    }
}
