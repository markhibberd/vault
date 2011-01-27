package com.ephox.vault

import java.util.Date
import java.sql.{ResultSet, Timestamp, Time}

trait Row {
  import scalaz._
  import Scalaz._
  import Row._
  import PossibleCell._

  def fold[A](
    row: List[(String, String, Cell)] => A
  ): A

  def ++(column: (String, String, Cell)) =
    fold(r => row(r ++ List(column)))

  // FIX I have left this at 1 indexed, as per jdbc, consider change?
  def byIndex(index: Int): PossibleCell =
    fold(row => {
        val d = row.drop(index - 1)
        if (d.isEmpty) nocell else somecell(d.head._3)
      })

  def byName(name: String): PossibleCell =
    find((_, n, _) => name == n)

  def byTableName(table: String, name: String): PossibleCell =
    find((t, n, _) => (table == t) && (name == n))

  def find(f: (String, String, Cell) => Boolean): PossibleCell =
    fold(row => row.find({case (t, n, c) => f(t, n, c)}).fold(c => somecell(c._3), nocell))
}

object Row {
  def empty = row(List())

  def row(value: List[(String, String, Cell)]): Row = new Row {
    def fold[A](
      row: List[(String, String, Cell)] => A
    ): A = row(value)
  }
}
