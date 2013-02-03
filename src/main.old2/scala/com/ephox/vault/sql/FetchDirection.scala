package com.ephox
package vault
package sql
import java.sql.{ResultSet => R}

sealed trait FetchDirection {
  import FetchDirection._
  def int: Int =
    this match {
      case Forward => R.FETCH_FORWARD
      case Reverse => R.FETCH_REVERSE
      case Unknown => R.FETCH_UNKNOWN
    }
}

object FetchDirection {
  case object Forward extends FetchDirection
  case object Reverse extends FetchDirection
  case object Unknown extends FetchDirection
}