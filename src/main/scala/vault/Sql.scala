package vault

import java.sql._
import java.net.URL

trait Sql {
  def byte: Int => Byte => DbValue[Unit]
  def short: Int => Short => DbValue[Unit]
  def int: Int => Int => DbValue[Unit]
  def long: Int => Long => DbValue[Unit]
  def float: Int => Float => DbValue[Unit]
  def double: Int => Double => DbValue[Unit]
  def string: Int => String => DbValue[Unit]
  def boolean: Int => Boolean => DbValue[Unit]
  def bigdecimal: Int => BigDecimal => DbValue[Unit]
  def date: Int => Date => DbValue[Unit]
  def time: Int => Time => DbValue[Unit]
  def timestamp: Int => Timestamp => DbValue[Unit]
  def url: Int => URL => DbValue[Unit]

  def toBind(n: Int): BindParam =
    BindParam(n, this)
}

case class BindParam(n: Int, sql: Sql) {
  def byte = sql.byte(n)
  def short = sql.short(n)
  def int = sql.int(n)
  def long = sql.long(n)
  def float = sql.float(n)
  def double = sql.double(n)
  def string = sql.string(n)
  def boolean = sql.boolean(n)
  def bigdecimal = sql.bigdecimal(n)
  def date = sql.date(n)
  def time = sql.time(n)
  def timestamp = sql.timestamp(n)
  def url = sql.url(n)
}

object Sql {
  def jdbc(s: PreparedStatement): Sql = new Sql {
    def byte = set(_.setByte(_, _))
    def short = set(_.setShort(_, _))
    def int = set(_.setInt(_, _))
    def long = set(_.setLong(_, _))
    def float = set(_.setFloat(_, _))
    def double = set(_.setDouble(_, _))
    def string = set(_.setString(_, _))
    def boolean = set(_.setBoolean(_, _))
    def bigdecimal = set((s, n, v) => s.setBigDecimal(n, v.bigDecimal))
    def date = set(_.setDate(_, _))
    def time = set(_.setTime(_, _))
    def timestamp = set(_.setTimestamp(_, _))
    def url = set(_.setURL(_, _))

    def set[A](f: (PreparedStatement, Int, A) => Unit): Int => A =>  DbValue[Unit] =
      n => a => try {
        f(s, n, a)
        DbValue.ok(Unit)
      } catch {
        case e: SQLException =>
          DbValue.exception(e)
      }
  }
}
