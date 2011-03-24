package com.ephox.vault

trait SqlQuery {
  def fold[X](
    sql: (String, List[JDBCType]) => X
  ): X
}

trait SqlQuerys {
  def sql(sql: String, bindings: List[JDBCType] = Nil): SqlQuery = new SqlQuery {
    def fold[X](
      bound: (String, List[JDBCType]) => X
    ): X = bound(sql, bindings)
  }
}