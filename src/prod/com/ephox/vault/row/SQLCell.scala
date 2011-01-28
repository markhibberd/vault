package com.ephox.vault.row

import java.math.BigDecimal
import java.sql.{SQLException, ResultSet, Timestamp, Time, Date}
import com.ephox.vault2.SQLValue

trait SQLCell {
  def fold[A](
    nul: => A,
    boolean: Boolean => A,
    string: String => A,
    byte: Byte => A,
    short: Short => A,
    int: Int => A,
    long: Long => A,
    float: Float => A,
    double: Double => A,
    bigdecimal: BigDecimal => A,
    date: Date => A,
    time: Time => A,
    timestamp: Timestamp => A): A
}

object SQLCell {
  import scalaz._
  import Scalaz._

  def nul: SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = nul
  }

  def boolean(value: Boolean): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = boolean(value)
  }


  def string(value: String): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = string(value)
  }

  def byte(value: Byte): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = byte(value)
  }

  def short(value: Short): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = short(value)
  }

  def int(value: Int): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = int(value)
  }

  def long(value: Long): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = long(value)
  }

  def float(value: Float): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = float(value)
  }

  def double(value: Double): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = double(value)
  }

  def bigdecimal(value: BigDecimal): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = bigdecimal(value)
  }

  def date(value: Date): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = date(value)
  }

  def time(value: Time): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = time(value)
  }

  def timestamp(value: Timestamp): SQLCell = new SQLCell {
    def fold[A](
      nul: => A,
      boolean: Boolean => A,
      string: String => A,
      byte: Byte => A,
      short: Short => A,
      int: Int => A,
      long: Long => A,
      float: Float => A,
      double: Double => A,
      bigdecimal: BigDecimal => A,
      date: Date => A,
      time: Time => A,
      timestamp: Timestamp => A): A = timestamp(value)
  }

  val oBoolean = (value: Option[Boolean]) => value.fold(boolean(_), nul)
  val oString = (value: Option[String]) => value.fold(string(_), nul)
  val oByte = (value: Option[Byte]) => value.fold(byte(_), nul)
  val oShort = (value: Option[Short]) => value.fold(short(_), nul)
  val oInt = (value: Option[Int]) => value.fold(int(_), nul)
  val oLong = (value: Option[Long]) => value.fold(long(_), nul)
  val oFloat = (value: Option[Float]) => value.fold(float(_), nul)
  val oDouble = (value: Option[Double]) => value.fold(double(_), nul)
  val oBigDecimal = (value: Option[BigDecimal]) => value.fold(bigdecimal(_), nul)
  val oDate = (value: Option[Date]) => value.fold(date(_), nul)
  val oTime = (value: Option[Time]) => value.fold(time(_), nul)
  val oTimestamp = (value: Option[Timestamp]) => value.fold(timestamp(_), nul)

  def read(rs: ResultSet, i: Int): SQLValue[SQLCell] = {
    import SQLCellReader._

    def reader[A](implicit reader: SQLCellReader[A]): SQLValue[Option[A]] = SQLValue.value(reader.read(rs, i))

    try {
      rs.getMetaData.getColumnClassName(i) match {
        case "java.lang.Boolean" => reader[Boolean] ∘ oBoolean
        case "java.lang.String" => reader[String] ∘ oString
        case "java.lang.Byte" => reader[Byte] ∘ oByte
        case "java.lang.Short" => reader[Short] ∘ oShort
        case "java.lang.Integer" => reader[Int] ∘ oInt
        case "java.lang.Long" => reader[Long] ∘ oLong
        case "java.lang.Float" => reader[Float] ∘ oFloat
        case "java.lang.Double" => reader[Double] ∘ oDouble
        case "java.math.BigDecimal" => reader[java.math.BigDecimal] ∘ oBigDecimal
        case "java.sql.Date" => reader[Date] ∘ oDate
        case "java.sql.Time" => reader[Time] ∘ oTime
        case "java.sql.Timestamp" => reader[Timestamp] ∘ oTimestamp
        case x => SQLValue.err(new SQLException("Unknown/unsupported type [" + x + "]."))
      }
    } catch {
      case e: SQLException => SQLValue.err(e)
    }
  }

}
