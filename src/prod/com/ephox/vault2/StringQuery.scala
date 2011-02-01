package com.ephox.vault2

import java.sql.{PreparedStatement, ResultSet, Statement}

sealed trait StringQuery {
  val sql: String

  def executeQuery[A](f: ResultSetConnector[A]): Connector[A] =
    connector(c => withSQLResource(
                     value = c prepareStatement sql
                   , evaluate = (s: PreparedStatement) =>
                       withSQLResource(
                         value    = s.executeQuery
                       , evaluate = (r: ResultSet) => f(r)(c)
                       )
                   ))

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