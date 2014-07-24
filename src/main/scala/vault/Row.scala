package vault

import java.net.URL
import java.sql._

trait Row {
  def byte: Int => DbValue[Option[Byte]]
  def short: Int => DbValue[Option[Short]]
  def int: Int => DbValue[Option[Int]]
  def long: Int => DbValue[Option[Long]]
  def float: Int => DbValue[Option[Float]]
  def double: Int => DbValue[Option[Double]]
  def string: Int => DbValue[Option[String]]
  def boolean: Int => DbValue[Option[Boolean]]
  def bigdecimal: Int => DbValue[Option[BigDecimal]]
  def date: Int => DbValue[Option[Date]]
  def time: Int => DbValue[Option[Time]]
  def timestamp: Int => DbValue[Option[Timestamp]]

  def toCell(n: Int): Cell =
    Cell(n, this)
}

case class Cell(n: Int, row: Row) {
  def byte = row.byte(n)
  def short = row.short(n)
  def int = row.int(n)
  def long = row.long(n)
  def float = row.float(n)
  def double = row.double(n)
  def string = row.string(n)
  def boolean = row.boolean(n)
  def bigdecimal = row.bigdecimal(n)
  def date = row.date(n)
  def time = row.time(n)
  def timestamp = row.timestamp(n)
}

object Row {
  def jdbc(r: ResultSet): Row = new Row {
    def byte = get(_.getByte(_))
    def short = get(_.getShort(_))
    def int = get(_.getInt(_))
    def long = get(_.getLong(_))
    def float = get(_.getFloat(_))
    def double = get(_.getDouble(_))
    def string = get(_.getString(_))
    def boolean = get(_.getBoolean(_))
    def bigdecimal = get(_.getBigDecimal(_))
    def date = get(_.getDate(_))
    def time = get(_.getTime(_))
    def timestamp = get(_.getTimestamp(_))

    /* Turn an unsafe get* on ResultSet into a safe one. */
    def get[A](f: (ResultSet, Int) => A): Int => DbValue[Option[A]] =
      n => try {
        val v = f(r, n)
        if (r.wasNull)
          DbValue.ok(None)
        else
          DbValue.ok(Some(v))
      } catch {
        case e: SQLException =>
          DbValue.exception(e)
      }
  }
}
