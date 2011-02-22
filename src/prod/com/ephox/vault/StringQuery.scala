package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val sql: String

  def executeUpdate: Connector[Int] =
    connector(c => withSQLResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       trySQLValue(s executeUpdate sql)
                   ))

  def executeUpdateWithKeys[A, B](withStatement: PreparedStatement => A, withRow: Row => A => Int => Connector[B]): Connector[B] =
    connector(c => withSQLResource(
                     value = c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
                   , evaluate = (s: PreparedStatement) => {
                         val a = withStatement(s)
                         val o = for {
                           b <- {
                             val n = s.executeUpdate
                             val r = s.getGeneratedKeys
                             withRow(Row.resultSetRow(r))(a)(n)
                           }
                         } yield b
                         o(c)
                     }
                   ))

  def executeUpdateWithKeysSet[B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B): Connector[B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_)
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[Connector]
    )


  def prepareStatement[A](k: PreparedStatement => Connector[A]) : Connector[A] =
    connector(c => withSQLResource(c prepareStatement sql, (s: PreparedStatement) => k(s)(c)))
}

trait StringQuerys {
  implicit def StringStringQuery(s: String): StringQuery = new StringQuery {
    val sql = s
  }

  implicit def StringQueryString(sql: StringQuery): String =
    sql.sql
}