package com.ephox.vault

import java.util.Date
import java.sql.{Timestamp, Time}

trait Row {
  import scalaz._
  import Scalaz._
  import Row._
  import MaybeColumn._

  def fold[A](
    row: List[(String, String, Column)] => A
  ): A

  def ++(col: (String, String, Column)) =
    fold(r => row(r ++ List(col)))

  // FIX I have left this at 1 indexed, as per jdbc, consider change?
  def byIndex(index: Int): MaybeColumn =
    fold(row => {
        val d = row.drop(index - 1)
        if (d.isEmpty) noColumn else someColumn(d.head._3)
      })

  def byName(name: String): MaybeColumn =
    find((_, n, _) => name == n)

  def byTableName(table: String, name: String): MaybeColumn =
    find((t, n, _) => (table == t) && (name == n))

  def find(f: (String, String, Column) => Boolean): MaybeColumn =
    fold(row => row.find({case (t, n, c) => f(t, n, c)}).fold(c => someColumn(c._3), noColumn))
}

object Row {
  def empty = row(List())

  def row(value: List[(String, String, Column)]): Row = new Row {
    def fold[A](
      row: List[(String, String, Column)] => A
    ): A = row(value)
  }
}
