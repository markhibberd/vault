package vault

import scalaz._, Scalaz._, concurrent._, effect._
import java.sql.Connection

case class Db[+A](run: Context[A]) {
  def runLogDb(c: Connector): IO[(DbHistory, DbValue[A])] = c.create() |> { connection => for {
    r <- IO { run(connection).run.run.run }  onException (IO { connection.rollback })
    _ <- IO { connection.commit }
  } yield r }

  def runDb(c: Connector): IO[DbValue[A]] =
    runLogDb(c).map(_._2)

  def testLogDb(c: Connector): IO[(DbHistory, DbValue[A])] =
    c.create() |> { connection => IO { run(connection).run.run.run } ensuring (IO { connection.rollback }) }

  def testDb(c: Connector): IO[DbValue[A]] =
    testLogDb(c).map(_._2)

  def map[B](f: A => B): Db[B] =
    flatMap(a => Db.value(f(a)))

  def flatMap[B](f: A => Db[B]) =
    Db.withConnection(connection => run(connection).flatMap(a =>
      f(a).run(connection)))
}

object Db {
  def value[A](a: A): Db[A] =
    Db(a.pure[Context])

  def withConnection[A](f: Connection => Context_[A]): Db[A] =
    Db(Kleisli(connection => f(connection)))

  def withConnectionX[A](f: Connection => Task[(DbHistory, DbValue[A])]): Db[A] =
    withConnection(connection =>
      DbValueT[Context__, A](
        WriterT[Task, DbHistory, DbValue[A]](
          f(connection))))

  def raw[A](f: Connection => A): Db[A] =
    safe(conn => DbValue.ok(f(conn)))

  def connection: Db[Connection] =
    raw(identity)

  def safe[A](f: Connection => DbValue[A]): Db[A] =
    safeWithLog(f.map(v => DbHistory.empty -> v))

  def delay[A](a: => A): Db[A] =
    liftTask(Task.delay { a })

  def now[A](a: => A): Db[A] =
    liftTask(Task.now { a })

  def safeWithLog[A](f: Connection => (DbHistory, DbValue[A])): Db[A] =
    Db.withConnectionX(connection =>
      Task.delay { f(connection) } )

  def liftIO[A](io: IO[A]) =
    Db.withConnectionX(_ =>
      Task.delay { DbHistory.empty -> DbValue.ok(io.unsafePerformIO) } )

  def liftTask[A](t: Task[A]) =
    Db.withConnectionX(_ =>
      t.map(a => DbHistory.empty -> DbValue.ok(a)))

  implicit def DbMonad: Monad[Db] = new Monad[Db] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: Db[A])(f: A => Db[B]) = m flatMap f
  }

  implicit def DbCatchable: Catchable[Db] = new Catchable[Db] {
    def attempt[A](db: Db[A]): Db[Throwable \/ A] =
      Db.withConnection(conn => DbValueT[Context__, Throwable \/ A](WriterT[Task, DbHistory, DbValue[Throwable \/ A]](db.run.run(conn).run.run.attempt.map({
        case -\/(t) => DbHistory.empty -> DbValue.ok(t.left)
        case \/-((h, v)) => h -> v.map(_.right[Throwable])
}))))

    def fail[A](err: Throwable): Db[A] =
      liftTask(Task.fail(err))
  }
}
