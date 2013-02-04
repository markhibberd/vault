package vault

import java.sql.{PreparedStatement, SQLException}

trait Sql {
  def int: Int => Int => DbValue[Unit]
  def long: Int => Long => DbValue[Unit]
  def string: Int => String => DbValue[Unit]
  def boolean: Int => Boolean => DbValue[Unit]

  def toBind(n: Int): BindParam =
    BindParam(n, this)
}

case class BindParam(n: Int, sql: Sql) {
  def int = sql.int(n)
  def long = sql.long(n)
  def string = sql.string(n)
  def boolean = sql.boolean(n)
}

object Sql {
  def jdbc(s: PreparedStatement): Sql = new Sql {
    def int = set(_.setInt(_, _))
    def long = set(_.setLong(_, _))
    def string = set(_.setString(_, _))
    def boolean = set(_.setBoolean(_, _))

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
