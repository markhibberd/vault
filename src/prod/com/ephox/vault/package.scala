package com.ephox

import scalaz._
import Scalaz._
import java.sql.SQLException

package object vault
  extends StringQuerys
  with PreparedStatementWs
  with SQLValues
  with RowAccesss
  with RowAccessors
  with RowConnectors
  with SQLRowAccesss
  with SQLQueries
  with Connectors
  with JDBCTypes
  with SQLTypes
  with DDL {

  def withSQLResource[T, R](
                          value: => T
                        , evaluate: T => SQLValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SQLValue[R] =
    withResource(value, evaluate, {
      case e: SQLException => sqlError(e)
      case e               => throw e
    }, whenClosing)
}