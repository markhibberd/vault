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

  def update[A: ToDb](sql: String, a: A): Db[Int] = Db.safe(conn =>
    DbValue.db({
      val stmt = conn.prepareStatement(sql)
      ToDb.execute[A](Sql.jdbc(stmt), a);
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

  def statement(sql: String): Db[PreparedStatement] =
    Db.connection.flatMap(conn => Db.delay { conn.prepareStatement(sql) })

  def run[A: ToDb](statement: PreparedStatement, a: A): Db[ResultSet] =
    Db.delay { ToDb.execute[A](Sql.jdbc(statement), a); statement.executeQuery }

  def close[A <: java.lang.AutoCloseable]: A => Db[Unit] =
    a => Db.liftTask(Task.delay { a.close })

  def process[A: ToDb, B: FromDb](sql: String, a: A): Process[Db, B] =
    resourceX(statement(sql))(close[PreparedStatement]) { s =>
      Db.value((resource(run(s, a))(close[ResultSet]) { rs =>
        Db.delay { if (rs.next) Row.jdbc(rs) else throw Process.End  }
      }).evalMap[Db, B](row => Db.safe(_ => FromDb.perform[B](row))))
    }.join

  def resource[R,O](acquire: Db[R])(
                    release: R => Db[Unit])(
                    step: R => Db[O]): Process[Db,O] = {
    import Process._
    def go(step: Db[O], onExit: Process[Db,O]): Process[Db,O] =
      await[Db,O,O](step) (
        o => emit(o) ++ go(step, onExit) // Emit the value and repeat
      , onExit                           // Release resource when exhausted
      , onExit)                          // or in event of error
    await(acquire)(r => {
      val onExit = eval(release(r)).drain
      go(step(r), onExit)
    }, halt, halt)
  }

  def resourceX[R,O](acquire: Db[R])(
                    release: R => Db[Unit])(
                    step: R => Db[O]): Process[Db,O] = {
    import Process._
    def go(step: Db[O], onExit: Process[Db,O]): Process[Db,O] =
      await[Db,O,O](step) (
        o => emit(o)                     // Emit the value once
      , onExit                           // Release resource when exhausted
      , onExit)                          // or in event of error
    await(acquire)(r => {
      val onExit = await(Db.liftTask(Task.now {}))(_ => eval(release(r)).drain)
      go(step(r), onExit)
    }, halt, halt)
  }

}
