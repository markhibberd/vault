package com.ephox
package vault

import java.sql.PreparedStatement

sealed trait PreparedStatementContext {
  val preparedStatement: PreparedStatement
  val parameters: Option[(JDBCType, Int)]
}

object PreparedStatementContext extends PreparedStatementContextFunctions

trait PreparedStatementContextFunctions {
  def preparedStatementContext(s: PreparedStatement, p: Option[(JDBCType, Int)]): PreparedStatementContext =
    new PreparedStatementContext {
      val preparedStatement = s
      val parameters = p
    }
}
