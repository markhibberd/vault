package com.ephox.vault2

import java.sql.PreparedStatement

sealed trait PreparedStatementW {
  val s: PreparedStatement

  def tryExecuteUpdate: SQLValue[Int] =
    tryValue(s.executeUpdate)
}

object PreparedStatementW {
  implicit def PreparedStatementPreparedStatementW(t: PreparedStatement): PreparedStatementW = new PreparedStatementW {
    val s = t
  }

  implicit def PreparedStatementWPreparedStatement(t: PreparedStatementW): PreparedStatement =
    t.s
}
