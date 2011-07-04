package com.ephox.vault

trait Sql {
  val sql: String
  val bindings: List[JDBCType]
}

object Sql extends Sqls

trait Sqls {
  def query(s: String, b: List[JDBCType]): Sql = new Sql {
    val sql = s
    val bindings = b
  }
}
