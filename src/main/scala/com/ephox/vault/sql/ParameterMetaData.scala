package com.ephox
package vault
package sql

sealed trait ParameterMetaData {
  private[sql] val x: java.sql.ParameterMetaData
}

object ParameterMetaData {
  def apply(xx: java.sql.ParameterMetaData): ParameterMetaData =
    new ParameterMetaData {
      val x = xx
    }
}
