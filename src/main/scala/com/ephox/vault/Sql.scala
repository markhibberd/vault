package com.ephox
package vault

import scalaz._, Scalaz._

sealed trait Sql {
  val sql: String
  val bindings: List[JDBCType]
}

object Sql extends SqlFunctions {
  def apply(s: String, b: List[JDBCType]): Sql = new Sql {
    val sql = s
    val bindings = b
  }
}

trait SqlFunctions {
  val sqlL: Sql @> String =
    Lens(s => Store(Sql(_, s.bindings), s.sql))

  val bindingsL: Sql @> List[JDBCType] =
    Lens(s => Store(Sql(s.sql, _), s.bindings))
}
