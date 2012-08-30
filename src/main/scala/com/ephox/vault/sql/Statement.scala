package com.ephox
package vault
package sql

import SqlT._
import scalaz._, Scalaz._

sealed trait Statement {
  private[sql] val x: java.sql.Statement

  def addBatch(sql: String): Sql[Unit] =
    Try(x.addBatch(sql))

  def cancel: Sql[Unit] =
    Try(x.cancel)

  def clearBatch: Sql[Unit] =
    Try(x.clearBatch)

  def clearWarnings: Sql[Unit] =
    Try(x.clearWarnings)

  def close: Sql[Unit] =
    Try(x.close)

  def execute(sql: String): Sql[Boolean] =
    Try(x.execute(sql))
}

object Statement {
  def apply(xx: java.sql.Statement): Statement =
    new Statement {
      val x = xx
    }
}
