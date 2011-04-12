package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import java.sql.{SQLException, PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  import SqlConnect._
  import PreparedStatementW._
  import Key._
  import Sql._
  import JDBCType._

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
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[SqlConnect]
    )

  // FIX comback and clean-up error handling, trying to get the right sematics at the moment.
  def insert[A](a: A, withStatement: PreparedStatement => Unit)(implicit keyed: Keyed[A]): SqlConnect[A] =
    keyed.get(a).fold(
      executeUpdateWithKeysSet(
        withStatement,
        r => i => (i, keyed.set(a, r.keyLabel("ID").getValueOr(nokey)))
      ).map(_._2),
      id => constantSqlConnect(sqlError(sqlExceptionContext(new SQLException("Can not insert. Key is already set: " + id))))
    )

  def delete[A](a: A)(implicit keyed: Keyed[A]): SqlConnect[Int] =
    deleteKey(keyed.get(a))

  def deleteKey(key: Key): SqlConnect[Int] =
    key.fold(
      constantSqlConnect(sqlError(sqlExceptionContext(new SQLException("Can not delete a key that is not set.")))),
      id => executePreparedUpdate(_.set(longType(id)))
    )

  def update[A](a: A, update: (A, PreparedStatement) => Unit)(implicit keyed: Keyed[A]): SqlConnect[Int] =
    keyed.get(a).fold(
      constantSqlConnect(sqlError(sqlExceptionContext(new SQLException("Can not update a key that is not set.")))),
      id => executePreparedUpdate(update(a, _))
    )

  def prepareStatement[A](k: PreparedStatement => SqlConnect[A]) : SqlConnect[A] =
    sqlConnect(c => withSqlResource(c prepareStatement query, (s: PreparedStatement) => k(s)(c)))

  def bindSql(bindings: JDBCType*) = Sql.query(query, bindings: _*)

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