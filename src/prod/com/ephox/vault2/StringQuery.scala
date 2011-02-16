package com.ephox.vault2

import scalaz._
import Scalaz._
import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val sql: String

  def executeUpdate: Connector[Int] =
    connector(c => withSQLResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       tryValue(s executeUpdate sql)
                   ))

  def executeUpdateWithKeys[A, B](withStatement: PreparedStatement => Connector[A], withRow: Row => A => Int => Connector[B]): Connector[B] =
    connector(c => withSQLResource(
                     value = c.prepareStatement(sql, Statement.RETURN_GENERATED_KEYS)
                   , evaluate = (s: PreparedStatement) => {
                         val o = for {
                           i <- withStatement(s)
                           b <- {
                             val z = s.executeUpdate
                             val k = s.getGeneratedKeys
                             withRow(Row.resultSetRow(k))(i)(z)
                           }
                         } yield b
                         o(c)
                     }
                   ))

  def executeUpdateWithKeysSet[A, B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B): Connector[B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_).η[Connector]
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[Connector]
    )


  def prepareStatement[A](k: PreparedStatement => Connector[A]) : Connector[A] =
    connector(c => withSQLResource(c prepareStatement sql, (s: PreparedStatement) => k(s)(c)))
}

object StringQuery {
  def stringQuery(s: String): StringQuery = new StringQuery {
    val sql = s
  }
}