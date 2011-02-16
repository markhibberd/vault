package com.ephox.vault2

import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val sql: String

  def executeUpdate: Connector[Int] =
    connector(c => withSQLResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       tryValue(s executeUpdate sql)
                   ))

  def executeUpdateWithKeys[A](f: Row => A): Connector[(Int, A)] =
    connector(c => withSQLResource(
                     value = c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
                   , evaluate = (s: PreparedStatement) => {
                       tryValue({
                         val c = s.executeUpdate
                         val k = s.getGeneratedKeys
                         (c, f(Row.resultSetRow(k)))
                       })
                     }
                   ))

  def prepareStatement[A](k: PreparedStatement => Connector[A]) : Connector[A] =
    connector(c => withSQLResource(c prepareStatement sql, (s: PreparedStatement) => k(s)(c)))
}

object StringQuery {
  def stringQuery(s: String): StringQuery = new StringQuery {
    val sql = s
  }
}