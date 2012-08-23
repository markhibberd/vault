package com.ephox
package vault

import SqlExceptionContext._
import scalaz._, Scalaz._

sealed trait SqlExceptionContext {
  val sqlException: SqlException
  val preparedStatementContext: Option[PreparedStatementContext]
  val query: Option[Sql]
}

object SqlExceptionContext extends SqlExceptionContextFunctions

trait SqlExceptionContextFunctions {
  type SqlException =
  java.sql.SQLException

  def sqlExceptionContext(e: SqlException, c: Option[PreparedStatementContext], q: Option[Sql]): SqlExceptionContext =
    new SqlExceptionContext {
      val sqlException = e
      val preparedStatementContext = c
      val query = q
    }

  val contextSqlExceptionL: SqlExceptionContext @> SqlException =
    Lens(s => Store(sqlExceptionContext(_, s.preparedStatementContext, s.query), s.sqlException))

  val statementSqlExceptionL: SqlExceptionContext @> Option[PreparedStatementContext] =
    Lens(s => Store(sqlExceptionContext(s.sqlException, _, s.query), s.preparedStatementContext))

  val querySqlExceptionL: SqlExceptionContext @> Option[Sql] =
    Lens(s => Store(sqlExceptionContext(s.sqlException, s.preparedStatementContext, _), s.query))

  val statementSqlExceptionPL: SqlExceptionContext @?> PreparedStatementContext =
    ~statementSqlExceptionL >=> PLensT.somePLens

  val querySqlExceptionPL: SqlExceptionContext @?> Sql =
    ~querySqlExceptionL >=> PLensT.somePLens

}