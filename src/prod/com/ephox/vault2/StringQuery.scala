package com.ephox.vault2

import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val sql: String

  def executeUpdate[A]: Connector[Int] =
    connector(c => withSQLResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       tryValue(s executeUpdate sql)
                   ))

  def prepareStatement[A](k: PreparedStatement => Connector[A]) : Connector[A] =
    connector(c => withSQLResource(c prepareStatement sql, (s: PreparedStatement) => k(s)(c)))
}

object StringQuery {
  def stringQuery(s: String): StringQuery = new StringQuery {
    val sql = s
  }
}