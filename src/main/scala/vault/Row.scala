package vault

import java.sql.{ResultSet, SQLException}

trait Row {
  def int: Int => DbValue[Option[Int]]
  def long: Int => DbValue[Option[Long]]
  def string: Int => DbValue[Option[String]]
  def boolean: Int => DbValue[Option[Boolean]]

  def toCell(n: Int): Cell =
    Cell(n, this)
}

case class Cell(n: Int, row: Row) {
  def int = row.int(n)
  def long = row.long(n)
  def string = row.string(n)
  def boolean = row.boolean(n)
}

object Row {
  def jdbc(r: ResultSet): Row = new Row {
    def int = get(_.getInt(_))
    def long = get(_.getLong(_))
    def string = get(_.getString(_))
    def boolean = get(_.getBoolean(_))

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
