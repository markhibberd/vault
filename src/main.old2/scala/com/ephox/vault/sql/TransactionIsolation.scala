package com.ephox
package vault
package sql

import java.sql.{Connection => C}

sealed trait TransactionIsolation {
  import TransactionIsolation._
  def int: Int =
    this match {
      case ReadUncommitted => C.TRANSACTION_READ_UNCOMMITTED
      case ReadCommitted => C.TRANSACTION_READ_COMMITTED
      case RepeatableRead => C.TRANSACTION_REPEATABLE_READ
      case Serializable => C.TRANSACTION_SERIALIZABLE
      case None => C.TRANSACTION_NONE
    }
}
object TransactionIsolation {
  case object ReadUncommitted extends TransactionIsolation
  case object ReadCommitted extends TransactionIsolation
  case object RepeatableRead extends TransactionIsolation
  case object Serializable extends TransactionIsolation
  case object None extends TransactionIsolation
}
