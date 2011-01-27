package com.ephox.vault

import java.sql._

trait CellReader[A] {
  def read(rs: ResultSet, i: Int): Option[A]
}

object CellReader {
  def cellReader[A](f: (ResultSet, Int) => A): CellReader[A] = new CellReader[A] {
    def read(rs: ResultSet, i: Int) = {
      val r = f(rs, i)
      if (rs.wasNull) None else Some(r)
    }
  }

  implicit def BooleanCellReader: CellReader[Boolean] = cellReader(_.getBoolean(_))
  implicit def StringCellReader: CellReader[String] = cellReader(_.getString(_))
  implicit def ByteCellReader: CellReader[Byte] = cellReader(_.getByte(_))
  implicit def ShortCellReader: CellReader[Short] = cellReader(_.getShort(_))
  implicit def IntegerCellReader: CellReader[Int] = cellReader(_.getInt(_))
  implicit def LongCellReader: CellReader[Long] = cellReader(_.getLong(_))
  implicit def FloatCellReader: CellReader[Float] = cellReader(_.getFloat(_))
  implicit def DoubleCellReader: CellReader[Double] = cellReader(_.getDouble(_))
  implicit def BigDecimalReader: CellReader[java.math.BigDecimal] = cellReader(_.getBigDecimal(_))
  implicit def DateCellReader: CellReader[Date] = cellReader(_.getDate(_))
  implicit def TimeCellReader: CellReader[Time] = cellReader(_.getTime(_))
  implicit def TimestampCellReader: CellReader[Timestamp] = cellReader(_.getTimestamp(_))
}