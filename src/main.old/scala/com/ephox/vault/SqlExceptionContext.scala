package com.ephox.vault

import SqlExceptionContext._
import PreparedStatementContext._
import java.sql.PreparedStatement

sealed trait SqlExceptionContext {
  val sqlException: SqlException
  val prepareStatementContext: Option[PreparedStatementContext]
  val query: Option[Sql]

  def detail =
    ("""Unexpected database error:
      | Vault sql:
      |    """ + (query match {
          case None => "no sql set."
          case Some(q) => "sql[" + q.sql + "], bindings[" + q.bindings.mkString(", ") + "]"
        }) + """
      | JDBC prepared statement:
      |    """ + (prepareStatementContext match {
          case None => "No prepared statement created."
          case Some(s) => "statement[" + s.preparedStatement + "], bindings[" + s.parameters + "]"
        }) + """
    """).stripMargin

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

  def setQueryPreparedStatement(q: Sql, p: java.sql.PreparedStatement): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(preparedStatementContext(p))
    val query = Some(q)
  }

  def setQueryPreparedStatementPS(q: Sql, p: java.sql.PreparedStatement, t: JDBCType, i: Int): SqlExceptionContext = new SqlExceptionContext {
    val sqlException = SqlExceptionContext.this.sqlException
    val prepareStatementContext = Some(preparedStatementContextPS(p, t, i))
    val query = Some(q)
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

  def sqlExceptionMessage(msg: String) =
    sqlExceptionContext(new SqlException(msg))

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
