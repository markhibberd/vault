package com.ephox.vault

sealed trait DdlStatement {
  def fold[A](
    ddls: List[String] => A
  ): A
}

object DdlStatement {
  def ddl(s: String) = ddls(List(s))

  def ddls(s: List[String]): DdlStatement = new DdlStatement {
    def fold[A](
      ddls: List[String] => A
    ): A = ddls(s)
  }
}