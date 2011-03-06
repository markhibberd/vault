package com.ephox.vault

trait SQLQuery {
  def fold[X](
    sql: (String, List[JDBCType]) => X
  ): X
}

trait SQLQueries {
  def sql(sql: String, bindings: List[JDBCType] = Nil): SQLQuery = new SQLQuery {
    def fold[X](
      bound: (String, List[JDBCType]) => X
    ): X = bound(sql, bindings)
  }
}