package com.ephox.vault

trait Query {
  val sql: String
  val bindings: List[JDBCType]
}

object Query extends Querys

trait Querys {
  def query(s: String, b: JDBCType*): Query = new Query {
    val sql = s
    val bindings = b.toList
  }
}