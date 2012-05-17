package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import java.sql.{Connection, SQLException, PreparedStatement, Statement}

sealed trait StringQuery {
  val query: String

  import SqlConnect._
  import PreparedStatementW._
  import CompanionKey._
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

  def executeUpdateWithPrepared[A, B](mkStatement: Connection => PreparedStatement, withStatement: PreparedStatement => A, withGeneratedKeys: Row => A => Int => SqlConnect[B], noGeneratedKeys: => SqlConnect[B], handle: SqlExceptionContext => SqlExceptionContext = x => x): SqlConnect[B] =
    sqlConnect(c => withSqlResource(
                     value = mkStatement(c)
                   , evaluate = (s: PreparedStatement) => {
                         val a = withStatement(s)
                         val o = for {
                           b <- {
                             val n = s.executeUpdate
                             val r = s.getGeneratedKeys
                             if (r.next)
                               withGeneratedKeys(Row.resultSetRow(r))(a)(n)
                             else
                               noGeneratedKeys
                           }
                         } yield b
                         o(c).mapError(handle)
                     }
                   ).mapError(handle))

  def insert[A](a: A, fields: List[JDBCType])(implicit keyed: Keyed[A]): SqlConnect[A] =
    keyed.get(a).fold(
      executeUpdateWithPrepared(
        mkStatement = (_: Connection).prepareStatement(query, Array(1)),
        withStatement =  (_: PreparedStatement).setValues(fields),
        withGeneratedKeys =  (r: Row) => (_: SqlValue[Unit]) => (i: Int) =>
          r.keyIndex(1).fold(
            e => constantSqlConnect(sqlErrorMessage("Error generating id [" + i + "], columns [" + r.columns.mkString(",") + "], query [" + query + "], bindings [" + fields.mkString(",") + "]" + e.detail)),
            x => x.point[SqlConnect],
            nul => constantSqlConnect(sqlErrorMessage("Null id generated [" + i + "], columns [" + r.columns.mkString(",") + "], query [" + query + "], bindings [" + fields.mkString(",") + "]"))
          ) map (keyed.set(a, _)),
        noGeneratedKeys = sys.error("No generating id for query [" + query + "], bindings [" + fields.mkString(",") + "]"): SqlConnect[A],
        handle = e => e.setQuery(Sql.query(query, fields))
      ) ,
      id => constantSqlConnect(sqlErrorMessage("Can not insert. Key is already set: " + id))
    )

  def delete[A](a: A)(implicit keyed: Keyed[A]): SqlConnect[Int] =
    deleteKey(keyed.get(a))

  def deleteKey(key: Key): SqlConnect[Int] =
    key.fold(
      constantSqlConnect(sqlErrorMessage("Can not delete a key that is not set.")),
      id => executePreparedUpdate(_.set(longType(id)), e => e.setQuery(Sql.query(query, longType(id) :: Nil)))
    )

  def update[A](a: A, fields: List[JDBCType])(implicit keyed: Keyed[A]): SqlConnect[A] =
    keyed.get(a).fold(
      constantSqlConnect(sqlErrorMessage("Can not update a key that is not set.")),
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
