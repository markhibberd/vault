package com.ephox.vault

import java.sql.PreparedStatement

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JDBCType, Int)]
}

object PreparedStatementContext extends PreparedStatementContexts

trait PreparedStatementContexts {
  def preparedStatementContext(e: PreparedStatement): PreparedStatementContext = new PreparedStatementContext {
    val preparedStatement = e
    val parameters = None
  }

  def preparedStatementContextPS(e: PreparedStatement, t: JDBCType, i: Int): PreparedStatementContext = new PreparedStatementContext {
    val preparedStatement = e
    val parameters = Some(t, i)
  }

}