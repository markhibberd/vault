package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import java.sql.{SQLException, PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  import SqlConnect._
  import PreparedStatementW._
  import CampanionKey._
  import Sql._
  import JDBCType._
  import SqlExceptionContext._

  def executeUpdate: SqlConnect[Int] =
    sqlConnect(c => withSqlResource(
                     value = c.createStatement
                   , evaluate = (s: Statement) => {
                       trySqlValue(s executeUpdate query).mapError(e => e.setQuery(Sql.query(query, Nil)))
            }))

  def executePreparedUpdate(withStatement: PreparedStatement => Unit, handle: SqlExceptionContext => SqlExceptionContext = x => x): SqlConnect[Int] =
    prepareStatement(s => sqlConnect(_ => {
      withStatement(s)
      s.tryExecuteUpdate
    }))

  def executeUpdateWithKeys[A, B](withStatement: PreparedStatement => A, withRow: Row => A => Int => SqlConnect[B], handle: SqlExceptionContext => SqlExceptionContext = x => x): SqlConnect[B] =
    sqlConnect(c => withSqlResource(
                     value = c.prepareStatement(query, Statement.RETURN_GENERATED_KEYS)
                   , evaluate = (s: PreparedStatement) => {
                         val a = withStatement(s)
                         val o = for {
                           b <- {
                             val n = s.executeUpdate
                             val r = s.getGeneratedKeys
                             if (!r.next) error("No key result set [" + n + "], columns [" + Row.resultSetRow(r).columns.mkString(",") + "]")
                             withRow(Row.resultSetRow(r))(a)(n)
                           }
                         } yield b
                         o(c).mapError(handle)
                     }
                   ).mapError(handle))

  def executeUpdateWithKeysSet[B](withStatement: PreparedStatement => Unit, withRow: Row => Int => B, handle: SqlExceptionContext => SqlExceptionContext = x => x): SqlConnect[B] =
    executeUpdateWithKeys(
      withStatement = withStatement(_)
    , withRow       = (r: Row) => (_: Unit) => (n: Int) => withRow(r)(n).η[SqlConnect]
    , handle        = handle
    )

  def insert[A](a: A, fields: List[JDBCType])(implicit keyed: Keyed[A]): SqlConnect[A] =
    keyed.get(a).fold(
      executeUpdateWithKeysSet(
        (_: PreparedStatement).setValues(fields),
        r => i => (i, keyed.set(a, r.keyIndex(1).fold(
          e => error("Error generating id [" + i + "], columns [" + r.columns.mkString(",") + "], query [" + query + "], bindings [" + fields.mkString(",") + "]" + e.detail),
          x => x,
          nul => error("Null id generated [" + i + "], columns [" + r.columns.mkString(",") + "], query [" + query + "], bindings [" + fields.mkString(",") + "]")
        ))),
        e => e.setQuery(Sql.query(query, fields))
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

  def update[A](a: A, fields: List[JDBCType])(implicit keyed: Keyed[A]): SqlConnect[A] =
    keyed.get(a).fold(
      constantSqlConnect(sqlError(sqlExceptionContext(new SQLException("Can not update a key that is not set.")))),
      id => executePreparedUpdate((_: PreparedStatement).setValues(fields), e => e.setQuery(Sql.query(query, fields))) map (_ => a)
    )

  def prepareStatement[A](k: PreparedStatement => SqlConnect[A], handle: SqlExceptionContext => SqlExceptionContext = x => x) : SqlConnect[A] =
    sqlConnect(c => withSqlResource(c prepareStatement query, (s: PreparedStatement) => k(s)(c)).mapError(handle))

  def bindValues(bindings: JDBCType*) = bind(bindings.toList)

  def bind(bindings: List[JDBCType]) = Sql.query(query, bindings)

  def toSql = bind(Nil)
}

object StringQuery extends StringQuerys

trait StringQuerys {
  implicit def StringStringQuery(s: String): StringQuery = new StringQuery {
    val query = s
  }

  implicit def StringQueryString(sql: StringQuery): String =
    sql.query
}
