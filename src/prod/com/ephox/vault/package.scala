package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends StringQuerys
  with PreparedStatementWs
  with SqlValues
  with RowValues
  with RowAccessors
  with RowConnects
  with SqlRowAccesss
  with SqlQuerys
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
      case e: SqlException => sqlError(e)
      case e               => throw e
    }, whenClosing)
}