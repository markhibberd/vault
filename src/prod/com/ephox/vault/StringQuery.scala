package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  def executeUpdate: SqlConnect[Int] =
    sqlConnect(c => withSQLResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       trySqlValue(s executeUpdate query)
                   ))

  def executePreparedUpdate(withStatement: PreparedStatement => Unit) =
    prepareStatement(s => sqlConnect(_ => {
      withStatement(s)
      s.tryExecuteUpdate
    }))

  def executeUpdateWithKeys[A, B](withStatement: PreparedStatement => A, withRow: Row => A => Int => SqlConnect[B]): SqlConnect[B] =
    sqlConnect(c => withSQLResource(
                     value = c.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)
                   , evaluate = (s: PreparedStatement) => {
                         val a = withStatement(s)
                         val o = for {
                           b <- {
                             val n = s.executeUpdate
                             val r = s.getGeneratedKeys
                             if (!r.next) error("Tony? How can we restructure this?")
                             withRow(Row.resultSetRow(r))(a)(n)
                           }
                         } yield b
                         o(c)
                     }
                   ))

  def executeUpdateWithKeysSet[B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B): SqlConnect[B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_)
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[SqlConnect]
    )

  def executeUpdateWithKey[A](a: A, withStatement: PreparedStatement => Unit)(implicit keyed: Keyed[A]): SqlConnect[A] =
    executeUpdateWithKeysSet(
      withStatement,
      r => i => (i, keyed.set(a, r.keyLabel("ID").getValueOr(Key.nokey)))
    ).map(_._2)

  def prepareStatement[A](k: PreparedStatement => SqlConnect[A]) : SqlConnect[A] =
    sqlConnect(c => withSQLResource(c prepareStatement query, (s: PreparedStatement) => k(s)(c)))

  def toSql = sql(query)

  def bindSql(bindings: JDBCType*) = sql(query, bindings.toList)
}

trait StringQuerys {
  implicit def StringStringQuery(s: String): StringQuery = new StringQuery {
    val query = s
  }

  implicit def StringQueryString(sql: StringQuery): String =
    sql.query
}