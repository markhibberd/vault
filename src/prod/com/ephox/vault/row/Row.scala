package com.ephox.vault.row

import java.sql.ResultSet
import com.ephox.vault2.SQLValue

trait Row {
  import scalaz._
  import Scalaz._
  import Row._

  def fold[A](
    row: List[(String, String, SQLCell)] => A
  ): A

  def ++(column: (String, String, SQLCell)) =
    fold(r => row(r ++ List(column)))

  def byIndex(index: Int): Option[(String, String, SQLCell)] =
    fold(row => row.drop(index - 1).headOption)

  def byName(name: String) =
    find((_, n, _) => name == n)

  def byTableName(table: String, name: String) =
    find((t, n, _) => (table == t) && (name == n))

  def find(f: (String, String, SQLCell) => Boolean): Option[(String, String, SQLCell)] =
    fold(_.find({case (t, n, c) => f(t, n, c)}))
}

object Row {
  import scalaz._
  import Scalaz._

  def empty: Row = row(List())

  def row(value: List[(String, String, SQLCell)]): Row = new Row {
    def fold[A](
      row: List[(String, String, SQLCell)] => A
    ): A = row(value)
  }

  def read(rs: ResultSet): SQLValue[Row] = {
    val meta = rs.getMetaData
    val count = meta.getColumnCount

    def foldl[A](z: A)(f: (A, SQLValue[(String, String, SQLCell)]) => A): A =
        (for (i <- 1 to count)
          yield SQLCell.read(rs, i) map (cell => (meta.getTableName(i), meta.getColumnName(i), cell))).foldl(z)(f)

    foldl(empty.pure[SQLValue])((acc, v) => acc >>= (row => v.map(cell => row ++ cell)))
  }
}
