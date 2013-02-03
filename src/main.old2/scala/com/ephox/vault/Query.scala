package com.ephox
package vault

import scalaz._, Scalaz._

sealed trait Query {
  val sql: String
  val bindings: List[JDBCType]
}

object Query extends QueryFunctions {
  def apply(s: String, b: List[JDBCType]): Query = new Query {
    val sql = s
    val bindings = b
  }
}

trait QueryFunctions {
  val queryL: Query @> String =
    Lens(s => Store(Query(_, s.bindings), s.sql))

  val bindingsL: Query @> List[JDBCType] =
    Lens(s => Store(Query(s.sql, _), s.bindings))
}
