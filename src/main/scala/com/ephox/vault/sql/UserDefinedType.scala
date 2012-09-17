package com.ephox
package vault
package sql

import java.sql.{Types => T}

sealed trait UserDefinedType {
  import UserDefinedType._
  def int: Int =
    this match {
      case JavaObject => T.JAVA_OBJECT
      case Struct => T.STRUCT
      case Distinct => T.DISTINCT
    }
}
object UserDefinedType {
  case object JavaObject extends UserDefinedType
  case object Struct extends UserDefinedType
  case object Distinct extends UserDefinedType
}
