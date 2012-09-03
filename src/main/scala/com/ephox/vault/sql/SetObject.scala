package com.ephox
package vault
package sql


sealed trait SetObject
object SetObject {
  case object None extends SetObject
  case class TargetType(t: SqlType) extends SetObject
  case class TargetTypeScale(t: SqlType, s: Int) extends SetObject

}