package com.ephox
package vault
package sql

import java.sql.{Connection => C}
import java.sql.{ResultSet => R}
import SqlT._
import XSqlT._

sealed trait Connection {
  private[sql] val x: java.sql.Connection

  import Connection._

  def clearWarnings: Sql[Unit] =
    Try(x.clearWarnings)

  def close: Sql[Unit] =
    Try(x.close)

  def commit: Sql[Unit] =
    Try(x.commit)

  def createStatement(s: CreateStatement): Sql[Statement] =
    Try(Statement(s.value match {
      case None => x.createStatement
      case Some((t, c, None)) => x.createStatement(t.int, c.int)
      case Some((t, c, Some(h))) => x.createStatement(t.int, c.int, h.int)
    }))

  def autoCommit: Sql[Boolean] =
    Try(x.getAutoCommit)

  def catalog: XSql[String] =
    TryNull(x.getCatalog)
}

object Connection {
  def apply(xx: java.sql.Connection): Connection =
    new Connection {
      val x = xx
    }

  sealed trait Transaction
  object Transaction {
    case object None extends Transaction
    case object ReadCommitted extends Transaction
    case object Uncommitted extends Transaction
    case object RepeatableRead extends Transaction
    case object Serializable extends Transaction
  }

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

  sealed trait ResultSetConcurrency {
    import ResultSetConcurrency._
    def int: Int =
      this match {
        case ReadOnly => R.CONCUR_READ_ONLY
        case ConcurUpdatable => R.CONCUR_UPDATABLE
      }
  }
  object ResultSetConcurrency {
    case object ReadOnly extends ResultSetConcurrency
    case object ConcurUpdatable extends ResultSetConcurrency
  }

  sealed trait ResultSetHoldability {
    import ResultSetHoldability._
    def int: Int =
      this match {
        case HoldsCursorsOverCommit => R.HOLD_CURSORS_OVER_COMMIT
        case CloseCursorsAtCommit => R.CLOSE_CURSORS_AT_COMMIT
      }
  }
  object ResultSetHoldability {
    case object HoldsCursorsOverCommit extends ResultSetHoldability
    case object CloseCursorsAtCommit extends ResultSetHoldability
  }

  sealed trait CreateStatement {
    val value: Option[(ResultSetType, ResultSetConcurrency, Option[ResultSetHoldability])]
  }

  object CreateStatement {
    def apply(): CreateStatement =
      new CreateStatement {
        val value = None
      }

    def typeConcurrency(t: ResultSetType, c: ResultSetConcurrency): CreateStatement =
      new CreateStatement {
        val value = Some(t, c, None)
      }

    def typeConcurrencyHoldability(t: ResultSetType, c: ResultSetConcurrency, h: ResultSetHoldability): CreateStatement =
      new CreateStatement {
        val value = Some(t, c, Some(h))
      }
  }
}
