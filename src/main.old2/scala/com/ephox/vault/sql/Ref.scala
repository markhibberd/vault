package com.ephox
package vault
package sql

sealed trait Ref {
  private[sql] val x: java.sql.Ref
}

object Ref {
  def apply(xx: java.sql.Ref): Ref =
    new Ref {
      val x = xx
    }
}
