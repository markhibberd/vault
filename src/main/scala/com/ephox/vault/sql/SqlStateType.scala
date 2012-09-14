package com.ephox
package vault
package sql

import java.sql.{DatabaseMetaData => D}

sealed trait SqlStateType {
  import SqlStateType._
  def int: Int =
    this match {
      case SqlXOpen => D.sqlStateXOpen
      case Sql99 => D.sqlStateSQL99
    }
}
object SqlStateType {
  case object SqlXOpen extends SqlStateType
  case object Sql99 extends SqlStateType
}
