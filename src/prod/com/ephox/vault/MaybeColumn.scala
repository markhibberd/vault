package com.ephox.vault

trait MaybeColumn {
  def fold[A](
    column: Column => A,
    nothing: => A
  ): A
}

object MaybeColumn {
  def noColumn: MaybeColumn = new MaybeColumn {
    def fold[A](
      column: Column => A,
      nothing: => A
    ): A = nothing
  }

  def someColumn(col: Column): MaybeColumn = new MaybeColumn {
    def fold[A](
      column: Column => A,
      nothing: => A
    ): A = column(col)
  }
}