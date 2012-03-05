package com.ephox.vault

import java.sql.PreparedStatement

import PreparedStatementContext._

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JDBCType, Int)]

  def setParameters(t: JDBCType, n: Int) =
    preparedStatementContextPS(preparedStatement, t, n)

  def unsetParameters =
    preparedStatementContext(preparedStatement)

  def withParameters(k: (JDBCType, Int) => (JDBCType, Int)) =
    parameters match {
      case None         => this
      case Some((p, q)) => k(p, q) match { case (x, y) => setParameters(x, y) }
    }
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