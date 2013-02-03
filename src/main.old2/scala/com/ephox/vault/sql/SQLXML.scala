package com.ephox
package vault
package sql

sealed trait SQLXML {
  private[sql] val x: java.sql.SQLXML
}

object SQLXML {
  def apply(xx: java.sql.SQLXML): SQLXML =
    new SQLXML {
      val x = xx
    }
}
