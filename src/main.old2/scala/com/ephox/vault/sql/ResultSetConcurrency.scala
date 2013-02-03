package com.ephox
package vault
package sql

import java.sql.{ResultSet => R}

sealed trait ResultSetConcurrency {
  import ResultSetConcurrency._
  def int: Int =
    this match {
      case ReadOnly => R.CONCUR_READ_ONLY
      case Updatable => R.CONCUR_UPDATABLE
    }
}
object ResultSetConcurrency {
  case object ReadOnly extends ResultSetConcurrency
  case object Updatable extends ResultSetConcurrency
}
