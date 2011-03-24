package com.ephox

import scalaz._
import Scalaz._
import java.sql.SQLException

package object vault
  extends StringQuerys
  with PreparedStatementWs
  with SqlValues
  with RowValues
  with RowAccessors
  with RowConnectors
  with SqlRowAccesss
  with SQLQueries
  with SqlConnects
  with JDBCTypes
  with SqlTypes
  with DDL {

  def withSQLResource[T, R](
                          value: => T
                        , evaluate: T => SqlValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SqlValue[R] =
    withResource(value, evaluate, {
      case e: SQLException => sqlError(e)
      case e               => throw e
    }, whenClosing)
}