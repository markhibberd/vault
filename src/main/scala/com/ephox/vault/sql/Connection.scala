package com.ephox
package vault
package sql

import java.sql.{Connection => C}
import java.sql.{ResultSet => R}
import SqlT._
import XSqlT._
import SqlError._
import scalaz._, Scalaz._
import collection.JavaConversions._

sealed trait Connection {
  private[sql] val x: java.sql.Connection

  import Connection._

  def clearWarnings: Sql[Unit] =
    Try(x.clearWarnings)

  def close: Sql[Unit] =
    Try(x.close)

  def commit: Sql[Unit] =
    Try(x.commit)

  def createStatement(s: ResultSety): Sql[Statement] =
    Try(Statement(s.value match {
      case None => x.createStatement
      case Some((t, c, None)) => x.createStatement(t.int, c.int)
      case Some((t, c, Some(h))) => x.createStatement(t.int, c.int, h.int)
    }))

  def autoCommit: Sql[Boolean] =
    Try(x.getAutoCommit)

  def catalog: XSql[String] =
    TryNull(x.getCatalog)

  def holdability: Sql[ResultSetHoldability] =
    Try(x.getHoldability) flatMap (c =>
      if(c == R.HOLD_CURSORS_OVER_COMMIT)
        SqlT.Value[Id, ResultSetHoldability](ResultSetHoldability.HoldsCursorsOverCommit)
      else if(c == R.CLOSE_CURSORS_AT_COMMIT)
        SqlT.Value[Id, ResultSetHoldability](ResultSetHoldability.CloseCursorsAtCommit)
      else
        SqlT.Error[Id, ResultSetHoldability](incompatibilityUnexpectedInt(c,
          """http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/Connection.html#getHoldability()
             Returns: the holdability, one of ResultSet.HOLD_CURSORS_OVER_COMMIT or ResultSet.CLOSE_CURSORS_AT_COMMIT""")))

  def metadata: Sql[DatabaseMetaData] =
    Try(DatabaseMetaData(x.getMetaData))

  def transactionIsolation: Sql[TransactionIsolation] =
    Try(x.getTransactionIsolation) flatMap (c =>
      if(c == C.TRANSACTION_READ_UNCOMMITTED)
        SqlT.Value[Id, TransactionIsolation](TransactionIsolation.ReadUncommitted)
      else if(c == C.TRANSACTION_READ_COMMITTED)
        SqlT.Value[Id, TransactionIsolation](TransactionIsolation.ReadCommitted)
      else if(c == C.TRANSACTION_REPEATABLE_READ)
        SqlT.Value[Id, TransactionIsolation](TransactionIsolation.RepeatableRead)
      else if(c == C.TRANSACTION_SERIALIZABLE)
        SqlT.Value[Id, TransactionIsolation](TransactionIsolation.Serializable)
      else if(c == C.TRANSACTION_NONE)
        SqlT.Value[Id, TransactionIsolation](TransactionIsolation.None)
      else
        SqlT.Error[Id, TransactionIsolation](incompatibilityUnexpectedInt(c,
          """http://docs.oracle.com/javase/1.5.0/docs/api/java/sql/Connection.html#getTransactionIsolation()
             Returns: the current transaction isolation level, which will be one of the following constants: Connection.TRANSACTION_READ_UNCOMMITTED, Connection.TRANSACTION_READ_COMMITTED, Connection.TRANSACTION_REPEATABLE_READ, Connection.TRANSACTION_SERIALIZABLE, or Connection.TRANSACTION_NONE.""")))

  def typeMap: Sql[collection.mutable.Map[String, Class[_]]] =
    Try(mapAsScalaMap(x.getTypeMap))

  def warnings: XSql[java.sql.SQLWarning] =
    TryNull(x.getWarnings)

  def isClosed: Sql[Boolean] =
    Try(x.isClosed)

  def isReadOnly: Sql[Boolean] =
    Try(x.isReadOnly)

  def nativeSql(sql: String): Sql[String] =
    Try(x.nativeSQL(sql))

  def prepareCall(sql: String, s: ResultSety): Sql[CallableStatement] =
    Try(CallableStatement(s.value match {
      case None => x.prepareCall(sql)
      case Some((t, c, None)) => x.prepareCall(sql, t.int, c.int)
      case Some((t, c, Some(h))) => x.prepareCall(sql, t.int, c.int, h.int)
    }))

  def prepareStatement(sql: String, s: PrepareStatement): Sql[PreparedStatement] =
    Try(PreparedStatement(s.value.fold(
      _.value match {
        case None => x.prepareStatement(sql)
        case Some((t, c, None)) => x.prepareStatement(sql, t.int, c.int)
        case Some((t, c, Some(h))) => x.prepareStatement(sql, t.int, c.int, h.int)
      }
    , _.fold(
        k => x.prepareStatement(sql, k)
      , _.fold(
          i => x.prepareStatement(sql, i.toArray)
        , n => x.prepareStatement(sql, n.toArray)
        )
      )
    )))

  def releaseSavepoint(savepoint: Savepoint): Sql[Unit] =
    Try(x.releaseSavepoint(savepoint.x))

  def rollback(savepoint: Option[Savepoint]): Sql[Unit] =
    Try(savepoint match {
      case None => x.rollback
      case Some(p) => x.rollback(p.x)
    })

  def setAutoCommit(autoCommit: Boolean): Sql[Unit] =
    Try(x.setAutoCommit(autoCommit))

  def setCatalog(catalog: String): Sql[Unit] =
    Try(x.setCatalog(catalog))

  def setHoldability(holdability: ResultSetHoldability): Sql[Unit] =
    Try(x.setHoldability(holdability.int))

  def setReadOnly(autoCommit: Boolean): Sql[Unit] =
    Try(x.setReadOnly(autoCommit))

  def setSavepoint(name: Option[String]): Sql[Savepoint] =
    Try(Savepoint(name match {
      case None => x.setSavepoint
      case Some(n) => x.setSavepoint(n)
    }))

  def setTransactionIsolation(level: TransactionIsolation): Sql[Unit] =
    Try(x.setTransactionIsolation(level.int))

  def setTypeMap(map: collection.mutable.Map[String, Class[_]]): Sql[Unit] =
    Try(x.setTypeMap(map))
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

  sealed trait ResultSety {
    val value: Option[(ResultSetType, ResultSetConcurrency, Option[ResultSetHoldability])]
  }

  object ResultSety {
    def apply(): ResultSety =
      new ResultSety {
        val value = None
      }

    def typeConcurrency(t: ResultSetType, c: ResultSetConcurrency): ResultSety =
      new ResultSety {
        val value = Some(t, c, None)
      }

    def typeConcurrencyHoldability(t: ResultSetType, c: ResultSetConcurrency, h: ResultSetHoldability): ResultSety =
      new ResultSety {
        val value = Some(t, c, Some(h))
      }
  }

  sealed trait PrepareStatement {
    val value: ResultSety \/ (Int \/ (List[Int] \/ List[String]))
  }

  object PrepareStatement {
    def apply(y: ResultSety): PrepareStatement =
      new PrepareStatement {
        val value = y.left
      }

    def autoGeneratedKeys(n: Int): PrepareStatement =
      new PrepareStatement {
        val value = n.left.right
      }

    def columnIndexes(x: List[Int]): PrepareStatement =
      new PrepareStatement {
        val value = x.left.right.right
      }

    def columnNames(x: List[String]): PrepareStatement =
      new PrepareStatement {
        val value = x.right.right.right
      }
  }
}
