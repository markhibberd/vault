package com.ephox.vault

import scalaz._
import Scalaz._
import SqlValue._
import java.sql.{PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  import SqlConnect._
  import PreparedStatementW._
  import Key._
  import Query._

  def executeUpdate: SqlConnect[Int] =
    sqlConnect(c => withSqlResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) =>
                       trySqlValue(s executeUpdate query)
                   ))

  def executePreparedUpdate(withStatement: PreparedStatement => Unit): SqlConnect[Int] =
    prepareStatement(s => sqlConnect(_ => {
      withStatement(s)
      s.tryExecuteUpdate
    }))

  def executeUpdateWithKeys[A, B](withStatement: PreparedStatement => A, withRow: Row => A => Int => SqlConnect[B]): SqlConnect[B] =
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

  def executeUpdateWithKeysSet[B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B): SqlConnect[B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_)
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[({type λ[α]= SqlConnect[α]})#λ]
    )

  def executeUpdateWithKey[A](a: A, withStatement: PreparedStatement => Unit)(implicit keyed: Keyed[A]): SqlConnect[A] =
    executeUpdateWithKeysSet(
      withStatement,
      r => i => (i, keyed.set(a, r.keyLabel("ID").getValueOr(nokey)))
    ).map(_._2)

  def prepareStatement[A](k: PreparedStatement => SqlConnect[A]) : SqlConnect[A] =
    sqlConnect(c => withSqlResource(c prepareStatement query, (s: PreparedStatement) => k(s)(c)))

  def bindSql(bindings: JDBCType*) = Query.query(query, bindings: _*)

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