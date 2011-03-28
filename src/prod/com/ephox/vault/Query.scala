package com.ephox.vault

trait Query {
  def fold[X](
    sql: (String, List[JDBCType]) => X
  ): X
}

trait Querys {
  def query(sql: String, bindings: List[JDBCType] = Nil): Query = new Query {
    def fold[X](
      bound: (String, List[JDBCType]) => X
    ): X = bound(sql, bindings)
  }
}