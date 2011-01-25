package com.ephox.vault

sealed trait DmlStatement {
  def fold[A](
    dml: (String, List[_]) => A
  ): A
}

object DmlStatement {
  def dml(s: String, l: List[_]): DmlStatement = new DmlStatement {
    def fold[A](
      dml: (String, List[_]) => A
    ) = dml(s, l)
  }
}