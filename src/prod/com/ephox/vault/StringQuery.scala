package com.ephox.vault

import scalaz._
import Scalaz._
import SqlValue._
import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  import SqlConnect._

  def executeUpdate[L]: SqlConnect[L, Int] =
    sqlConnect(c => withSqlResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       trySqlValue(s executeUpdate query)
                   ))

  def executePreparedUpdate[L](withStatement: PreparedStatement => Unit): SqlConnect[L, Int] =
    prepareStatement(s => sqlConnect(_ => {
      withStatement(s)
      s.tryExecuteUpdate
    }))

  def executeUpdateWithKeys[A, B, L](withStatement: PreparedStatement => A, withRow: Row => A => Int => SqlConnect[L, B]): SqlConnect[L, B] =
    sqlConnect(c => withSqlResource(
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

  def executeUpdateWithKeysSet[L, B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B): SqlConnect[L, B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_)
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[({type λ[α]= SqlConnect[L, α]})#λ]
    )

  def executeUpdateWithKey[L, A](a: A, withStatement: PreparedStatement => Unit)(implicit keyed: Keyed[A]): SqlConnect[L, A] =
    executeUpdateWithKeysSet(
      withStatement,
      r => i => (i, keyed.set(a, r.keyLabel("ID").getValueOr(nokey)))
    ).map(_._2)

  def prepareStatement[L, A](k: PreparedStatement => SqlConnect[L, A]) : SqlConnect[L, A] =
    sqlConnect(c => withSqlResource(c prepareStatement query, (s: PreparedStatement) => k(s)(c)))

  def bindSql(bindings: JDBCType*) = com.ephox.vault.query(query, bindings: _*)

  def toSql = bindSql()
}

object StringQuery extends StringQuerys

trait StringQuerys {
  implicit def StringStringQuery(s: String): StringQuery = new StringQuery {
    val query = s
  }

  implicit def StringQueryString(sql: StringQuery): String =
    sql.query
}