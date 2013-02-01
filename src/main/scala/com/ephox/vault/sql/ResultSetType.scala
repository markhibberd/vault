package com.ephox
package vault
package sql

import java.sql.{ResultSet => R}

sealed trait ResultSetType {
  import ResultSetType._
  def int: Int =
    this match {
      case ForwardOnly => R.TYPE_FORWARD_ONLY
      case ScrollInsensitive => R.TYPE_SCROLL_INSENSITIVE
      case ScrollSensitive => R.TYPE_SCROLL_SENSITIVE
    }
}
object ResultSetType {
  case object ForwardOnly extends ResultSetType
  case object ScrollInsensitive extends ResultSetType
  case object ScrollSensitive extends ResultSetType
}
