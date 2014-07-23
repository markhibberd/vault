package vault

import scalaz.stream._
import java.sql.{Connection, SQLException}
import scalaz._, Scalaz._, effect._, Effect._, concurrent._
import DbValue.db
import Db._

object Execute {
  def list[A: ToDb, B: FromDb](sql: String, a: A): Db[List[B]] =
    Db.withConnectionX(conn =>
      query[A, B](conn, sql, a).runLog.map(x => DbHistory.query(sql, a) -> x.toList.sequence))

  def list_[A: FromDb](sql: String): Db[List[A]] =
    list[Unit, A](sql, ())

  def get[A: ToDb, B: FromDb](sql: String, a: A): Db[Option[B]] =
    Db.withConnectionX(conn =>
      query[A, B](conn, sql, a).pipe(process1.once).runLog.map(x => DbHistory.query(sql, a) -> x.headOption.sequence))

  def get_[A: FromDb](sql: String): Db[Option[A]] =
    get[Unit, A](sql, ())

  def update_(sql: String): Db[Int] =
    update[Unit](sql, ())

  def update[A: ToDb](sql: String, a: A): Db[Int] = Db.safe(conn =>
    DbValue.db({
      val stmt = conn.prepareStatement(sql)
      stmt.executeUpdate
    }))

  def execute_(sql: String): Db[Boolean] =
    execute[Unit](sql, ())

  def execute[A: ToDb](sql: String, a: A): Db[Boolean] =
    Db.safeWithLog(conn =>
      DbHistory.execute(sql, a) -> DbValue.db({
        val stmt = conn.prepareStatement(sql);
        ToDb.execute[A](Sql.jdbc(stmt), a);
        stmt.execute
      }))

  def query[A: ToDb, B: FromDb](conn: Connection, sql: String, a: A): Process[Task, DbValue[B]] =
    io.resource(Task.delay(conn.prepareStatement(sql)))(stmt => Task.delay(stmt.close)) { stmt =>
      Task.now({
        ToDb.execute[A](Sql.jdbc(stmt), a)
        stmt.executeQuery

      }).flatMap(rs => Task.delay { if (rs.next) Row.jdbc(rs) else throw Process.End })
    }.map(row => FromDb.perform[B](row))
}
