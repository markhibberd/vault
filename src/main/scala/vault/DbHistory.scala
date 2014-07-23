package vault

sealed trait DbLog
case class ExecuteLog[A](sql: String, arg: A) extends DbLog
case class QueryLog[A](sql: String, arg: A) extends DbLog

case class DbHistory(history: Vector[DbLog])

object DbHistory {
  def empty: DbHistory =
    DbHistory(Vector())

  def execute[A](sql: String, arg: A): DbHistory =
    DbHistory(Vector(ExecuteLog(sql, arg)))

  def query[A](sql: String, arg: A): DbHistory =
    DbHistory(Vector(QueryLog(sql, arg)))

  import scalaz._
  implicit def DbHistoryMonoid: Monoid[DbHistory] = new Monoid[DbHistory] {
    def zero: DbHistory = DbHistory(Vector())
    def append(a: DbHistory, b: => DbHistory): DbHistory = DbHistory(a.history ++ b.history)
  }
}
