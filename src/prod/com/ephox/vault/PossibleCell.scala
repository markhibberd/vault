package com.ephox.vault

trait PossibleCell {
  def fold[A](
    cell: Cell => A,
    nothing: => A
  ): A
}

object PossibleCell {
  def nocell: PossibleCell = new PossibleCell {
    def fold[A](
      cell: Cell => A,
      nothing: => A
    ): A = nothing
  }

  def somecell(c: Cell): PossibleCell = new PossibleCell {
    def fold[A](
      cell: Cell => A,
      nothing: => A
    ): A = cell(c)
  }
}