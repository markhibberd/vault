package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql.SQLException

object Vault
  extends StringQuerys
  with PreparedStatementWs
  with SQLValues
  with LoggedSQLValues
  with RowAccesss
  with RowAccessors
  with RowConnectors
  with SQLRowAccesss
  with Connectors
  with JDBCTypes
  with SQLTypes
  with VaultIterV {

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