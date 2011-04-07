package com.ephox.vault

import SqlExceptionContext._

sealed trait SqlExceptionContext {
  val sqlException: SqlException

  val prepareStatementContext: Option[PreparedStatementContext]

  def setPreparedStatementContext(c: PreparedStatementContext) =
    sqlExceptionContextPS(sqlException, c)

  def unsetPreparedStatementContext =
    sqlExceptionContext(sqlException)

  def withPreparedStatementContext(k: PreparedStatementContext => PreparedStatementContext) =
    prepareStatementContext match {
      case None    => this
      case Some(c) => setPreparedStatementContext(k(c))
    }
}

object SqlExceptionContext extends SqlExceptionContexts

trait SqlExceptionContexts {
  type SqlException = java.sql.SQLException

  def sqlExceptionContext(e: SqlException): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = None
  }

  def sqlExceptionContextPS(e: SqlException, pc: PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = Some(pc)
  }
}
