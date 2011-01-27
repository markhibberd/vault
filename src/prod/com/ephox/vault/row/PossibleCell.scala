package com.ephox.vault.row

import com.ephox.vault2.SQLValue
import java.sql._

trait PossibleCell {
  def fold[A](
    cell: Cell => A,
    nothing: => A
  ): A
}

object PossibleCell {
  import Cell._
  import CellReader._
  import scalaz._
  import Scalaz._

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

  def read(rs: ResultSet, i: Int): SQLValue[PossibleCell] = {
    val meta = rs.getMetaData
    def reader[A](implicit reader: CellReader[A]): SQLValue[Option[A]] = SQLValue.value(reader.read(rs, i))
    if (meta.getColumnCount > i)
      SQLValue.value(nocell)
    else
      (meta.getColumnClassName(i) match {
        case "java.lang.Boolean" => reader[Boolean] ∘ (v => oBoolean(v))
        case "java.lang.String" => reader[String] ∘ (v => oString(v))
        case "java.lang.Byte" => reader[Byte] ∘ (v => oByte(v))
        case "java.lang.Short" => reader[Short] ∘ (v => oShort(v))
        case "java.lang.Integer" => reader[Int] ∘ (v => oInt(v))
        case "java.lang.Long" => reader[Long] ∘ (v => oLong(v))
        case "java.lang.Float" => reader[Float] ∘ (v => oFloat(v))
        case "java.lang.Double" => reader[Double] ∘ (v => oDouble(v))
        case "java.math.BigDecimal" => reader[java.math.BigDecimal] ∘ (v => oBigDecimal(v))
        case "java.sql.Date" => reader[Date] ∘ (v => oDate(v))
        case "java.sql.Time" => reader[Time] ∘ (v => oTime(v))
        case "java.sql.Timestamp" => reader[Timestamp] ∘ (v => oTimestamp(v))
        case x => SQLValue.err(new SQLException("Unknown/unsupported type [" + x + "]."))
      }) ∘ (v => somecell(v))
  }
}