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
  with Updates
  with KeyX
  with Keyeds
  with KeyedWX
  with Mergers
  with VaultIteratees {

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
