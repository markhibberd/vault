package com.ephox.vault

import SqlExceptionContext._

sealed trait SqlExceptionContext {
  val sqlException: SqlException
  val prepareStatementContext: Option[PreparedStatementContext]
  val query: Option[Sql]

  def setPreparedStatementContext(c: PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(c)
    val query = SqlExceptionContext.this.query
  }

  def unsetPreparedStatementContext: SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = None
    val query = SqlExceptionContext.this.query
  }

  def withPreparedStatementContext(k: PreparedStatementContext => PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext map k
    val query = SqlExceptionContext.this.query
  }

  def setQuery(q: Sql): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = Some(q)
  }

  def unsetQuery: SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = None
  }

  def withQuery(k: Sql => Sql): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = SqlExceptionContext.this.prepareStatementContext
    val query = SqlExceptionContext.this.query map k
  }
}

object SqlExceptionContext extends SqlExceptionContexts

trait SqlExceptionContexts {
  type SqlException = java.sql.SQLException

  def sqlExceptionContext(e: SqlException): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = None
    val query = None
  }

  def sqlExceptionContextPS(e: SqlException, pc: PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = Some(pc)
    val query = None
  }

  def sqlExceptionContextQuery(e: SqlException, q: Sql): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = None
    val query = Some(q)
  }

  def sqlExceptionContextQueryPS(e: SqlException, q: Sql, pc: PreparedStatementContext): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = e
    val prepareStatementContext = Some(pc)
    val query = Some(q)
  }
}
