package com.ephox.vault

sealed trait Query {
  def fold[A](
    prepared: (String, List[_]) => A
  ): A
}

object Query {
  def query(s: String, l: List[_] = List()): Sql = new Sql {
    def fold[A](
      prepared: (String, List[_]) => A
    ) = prepared(s, l)
  }
}