package com.ephox
package vault

import java.sql.PreparedStatement

sealed trait PreparedStatementWrap {
  val statement: PreparedStatement

  def context: PreparedStatementContext =
    PreparedStatementContext(statement, None)

  def contextWithParams(t: JDBCType, n: Int): PreparedStatementContext =
    PreparedStatementContext(statement, Some((t, n)))
}

object PreparedStatementWrap extends PreparedStatementWrapFunctions

trait PreparedStatementWrapFunctions {
  implicit def ToPreparedStatementWrap(s: PreparedStatement): PreparedStatementWrap =
    new PreparedStatementWrap {
      val statement = s
    }
}