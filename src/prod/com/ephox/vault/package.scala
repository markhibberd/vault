package com.ephox

import scalaz._
import Scalaz._

package object vault
  extends StringQuerys
  with PreparedStatementWs
  with RowValues
  with RowAccesss
  with RowConnects
  with SqlValues
  with SqlAccesss
  with SqlConnects
  with SqlQuerys
  with RowQuerys
  with JDBCTypes
  with SqlTypes
  with DDL {

  def withSqlResource[T, R, L](
                          value: => T
                        , evaluate: T => SqlValue[L, R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SqlValue[L, R] =
    withResource(value, evaluate, {
      case e: SqlException => sqlError(e)
      case e               => throw e
    }, whenClosing)
}