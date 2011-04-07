package com.ephox.vault

import SqlExceptionContext._

sealed trait SqlExceptionContext {
  val sqlException: SqlException

  val prepareStatementContext: Option[PreparedStatementContext]
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
