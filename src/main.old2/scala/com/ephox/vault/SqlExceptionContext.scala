package com.ephox
package vault

import SqlExceptionContext._
import scalaz._, Scalaz._

sealed trait SqlExceptionContext {
  val sqlException: SqlException
  val preparedStatementContext: Option[PreparedStatementContext]
  val query: Option[Query]
}

object SqlExceptionContext extends SqlExceptionContextFunctions {
  def apply(e: SqlException, c: Option[PreparedStatementContext], q: Option[Query]): SqlExceptionContext =
    new SqlExceptionContext {
      val sqlException = e
      val preparedStatementContext = c
      val query = q
    }
}

trait SqlExceptionContextFunctions {
  type SqlException =
  java.sql.SQLException

  val contextSqlExceptionL: SqlExceptionContext @> SqlException =
    Lens(s => Store(SqlExceptionContext(_, s.preparedStatementContext, s.query), s.sqlException))

  val statementSqlExceptionL: SqlExceptionContext @> Option[PreparedStatementContext] =
    Lens(s => Store(SqlExceptionContext(s.sqlException, _, s.query), s.preparedStatementContext))

  val querySqlExceptionL: SqlExceptionContext @> Option[Query] =
    Lens(s => Store(SqlExceptionContext(s.sqlException, s.preparedStatementContext, _), s.query))

  val statementSqlExceptionPL: SqlExceptionContext @?> PreparedStatementContext =
    ~statementSqlExceptionL >=> PLensT.somePLens

  val querySqlExceptionPL: SqlExceptionContext @?> Query =
    ~querySqlExceptionL >=> PLensT.somePLens

}