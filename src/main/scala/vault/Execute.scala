package vault

import scalaz.stream._
import java.sql.{Connection, SQLException, PreparedStatement, ResultSet}
import scalaz._, Scalaz._, effect._, Effect._, concurrent._
import DbValue.db
import Db._

object Execute {
  def list[A: ToDb, B: FromDb](sql: String, a: A): Db[List[B]] =
    process[A, B](sql, a).runLog.map(_.toList)

  def list_[A: FromDb](sql: String): Db[List[A]] =
    list[Unit, A](sql, ())

  def get[A: ToDb, B: FromDb](sql: String, a: A): Db[Option[B]] =
    process[A, B](sql, a).runLog.map(_.headOption)

  def get_[A: FromDb](sql: String): Db[Option[A]] =
    get[Unit, A](sql, ())

  def update_(sql: String): Db[Int] =
    update[Unit](sql, ())

  def update[A: ToDb](sql: String, a: A): Db[Int] = Db.withConnectionX(conn => Task.delay {
    DbValue.db({
      val stmt = conn.prepareStatement(sql)
      ToDb.execute[A](Sql.jdbc(stmt), a);
      stmt.executeUpdate
    }) })

  def execute_(sql: String): Db[Boolean] =
    execute[Unit](sql, ())

  def execute[A: ToDb](sql: String, a: A): Db[Boolean] =
    Db.withConnectionX(conn => Task.delay {
      DbValue.db({
        val stmt = conn.prepareStatement(sql);
        ToDb.execute[A](Sql.jdbc(stmt), a);
        stmt.execute
      }) })

  import Process._

  /* this is horrible, and I need to fix it, but the hackery is currently required to get anything near decent performance out... */
  def process[A: ToDb, B: FromDb](sql: String, a: A): Process[Db, B] = {
    def statement(sql: String): Db[PreparedStatement] =
      Db.withConnectionX(conn => Task.delay { DbValue.ok(conn.prepareStatement(sql)) })

    def run[A: ToDb](statement: PreparedStatement, a: A): Db[ResultSet] =
      Db.delay { ToDb.execute[A](Sql.jdbc(statement), a); statement.executeQuery }

    def close[A <: java.lang.AutoCloseable]: A => Db[Unit] =
      a => Db.liftTask(Task.delay { a.close })

    await(statement(sql))(s =>
      await(run(s, a))(rs =>
        await(Db.getChunkSize)(size => {

          import scala.collection.mutable._

          val buffer: ArrayBuffer[B] = new ArrayBuffer[B](size)
          var failure: DbFailure = null
          var done = false

          def chunk: ArrayBuffer[B] = {
            if (done)
              throw Process.End

            buffer.clear
            var i = 0; while (i < size && !done) {
              if (rs.next) {
                FromDb.perform[B](Row.jdbc(rs)) match {
                  case DbOk(b) => buffer += b
                  case DbErr(f) => done = true; failure = f
                }
              } else done = true
              i += 1
            }
            buffer
          }

          def next = Db.delayDbValue {
            val r = chunk
            if (failure != null)
              DbErr(failure)
            else
              DbOk(r)
          }

          def go: Process[Db, B] =
            await(next)(bs => emitAll(bs) ++ go)

          go
        }
      ), eval(close(s)).drain, eval(close(s)).drain))
  }
}
