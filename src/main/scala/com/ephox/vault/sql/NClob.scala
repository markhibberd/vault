package com.ephox
package vault
package sql

sealed trait NClob {
  private[sql] val x: java.sql.NClob
}

object NClob {
  def apply(xx: java.sql.NClob): NClob =
    new NClob {
      val x = xx
    }
}
