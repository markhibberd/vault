package com.ephox
package vault

import scalaz._, Scalaz._

sealed trait Sql {
  val sql: String
  val bindings: List[JDBCType]
}

object Sql extends SqlFunctions

trait SqlFunctions {
  def query(s: String, b: List[JDBCType]): Sql = new Sql {
    val sql = s
    val bindings = b
  }

  val sqlL: Sql @> String =
    Lens(s => Store(query(_, s.bindings), s.sql))

  val bindingsL: Sql @> List[JDBCType] =
    Lens(s => Store(query(s.sql, _), s.bindings))
}
