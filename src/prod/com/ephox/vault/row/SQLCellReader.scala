package com.ephox.vault.row

import java.sql._

trait SQLCellReader[A] {
  def read(rs: ResultSet, i: Int): Option[A]
}

object SQLCellReader {
  def sqlCellReader[A](f: (ResultSet, Int) => A): SQLCellReader[A] = new SQLCellReader[A] {
    def read(rs: ResultSet, i: Int) = {
      val r = f(rs, i)
      if (rs.wasNull) None else Some(r)
    }
  }

  implicit def BooleanSQLCellReader: SQLCellReader[Boolean] = sqlCellReader(_.getBoolean(_))
  implicit def StringSQLCellReader: SQLCellReader[String] = sqlCellReader(_.getString(_))
  implicit def ByteSQLCellReader: SQLCellReader[Byte] = sqlCellReader(_.getByte(_))
  implicit def ShortSQLCellReader: SQLCellReader[Short] = sqlCellReader(_.getShort(_))
  implicit def IntegerSQLCellReader: SQLCellReader[Int] = sqlCellReader(_.getInt(_))
  implicit def LongSQLCellReader: SQLCellReader[Long] = sqlCellReader(_.getLong(_))
  implicit def FloatSQLCellReader: SQLCellReader[Float] = sqlCellReader(_.getFloat(_))
  implicit def DoubleSQLCellReader: SQLCellReader[Double] = sqlCellReader(_.getDouble(_))
  implicit def BigDecimalSQLCellReader: SQLCellReader[java.math.BigDecimal] = sqlCellReader(_.getBigDecimal(_))
  implicit def DateSQLCellReader: SQLCellReader[Date] = sqlCellReader(_.getDate(_))
  implicit def TimeSQLCellReader: SQLCellReader[Time] = sqlCellReader(_.getTime(_))
  implicit def TimestampSQLCellReader: SQLCellReader[Timestamp] = sqlCellReader(_.getTimestamp(_))
}