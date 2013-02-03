package com.ephox
package vault
package sql

sealed trait Blob {
  private[sql] val x: java.sql.Blob
}

object Blob {
  def apply(xx: java.sql.Blob): Blob =
    new Blob {
      val x = xx
    }
}
