package vault

import scalaz._, Scalaz._, concurrent._, effect._
import java.sql.Connection

case class Db[+A](run: Context[A]) {
  def runLogDb(c: Connector): IO[(DbHistory, DbValue[A])] = c.create() |> { connection => for {
    b <- IO { scala.collection.mutable.ArrayBuffer[DbLog]() }
    r <- IO { run(DbRead.capture(connection, b)).run.run }  onException (IO { connection.rollback })
    _ <- IO { connection.commit }
  } yield DbHistory(b.toVector) -> r }

  def runDb(c: Connector): IO[DbValue[A]] = c.create() |> { connection => for {
    r <- IO { run(DbRead.connect(connection)).run.run }  onException (IO { connection.rollback })
    _ <- IO { connection.commit }
  } yield r }

  def testLogDb(c: Connector): IO[(DbHistory, DbValue[A])] =
    c.create() |> { connection => for {
      b <- IO { scala.collection.mutable.ArrayBuffer[DbLog]() }
      r <- IO { run(DbRead.capture(connection, b)).run.run } ensuring (IO { connection.rollback })
    } yield DbHistory(b.toVector) -> r }

  def testDb(c: Connector): IO[DbValue[A]] =
    c.create() |> { connection =>
      IO { run(DbRead.connect(connection)).run.run } ensuring (IO { connection.rollback }) }

  def map[B](f: A => B): Db[B] =
    flatMap(a => Db.value(f(a)))

  def flatMap[B](f: A => Db[B]) =
    Db.withDbRead(r => run(r).flatMap(a => f(a).run(r)))
}

object Db {
  def value[A](a: A): Db[A] =
    Db(a.pure[Context])

  def getChunkSize: Db[Int] =
    withDbRead(r => DbValueT[Task, Int](Task.now(DbValue.ok(r.chunk))))

  def withConnection[A](f: Connection => DbValueT[Task, A]): Db[A] =
    withDbRead(r => f(r.connection))

  def withDbRead[A](f: DbRead => DbValueT[Task, A]): Db[A] =
    Db(Kleisli[Context_, DbRead, A](r => f(r)))

  def withConnectionX[A](f: Connection => Task[DbValue[A]]): Db[A] =
    withConnection(connection =>
      DbValueT[Task, A](f(connection)))

  def delay[A](a: => A): Db[A] =
    liftTask(Task.delay { a })

  def now[A](a: => A): Db[A] =
    liftTask(Task.now { a })

  def liftIO[A](io: IO[A]) =
    liftTask(Task.delay { DbValue.ok(io.unsafePerformIO) })

  def liftTask[A](t: Task[A]) =
    Db.withConnectionX(_ => t.map(DbValue.ok))

  def liftDbValue[A](v: DbValue[A]) =
    Db.withConnection(_ => DbValueT[Task, A](Task.now(v)))

  def delayDbValue[A](v: => DbValue[A]) =
    Db.withConnection(_ => DbValueT[Task, A](Task.delay(v)))

  implicit def DbMonad: Monad[Db] = new Monad[Db] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: Db[A])(f: A => Db[B]) = m flatMap f
  }

  implicit def DbCatchable: Catchable[Db] = new Catchable[Db] {
    def attempt[A](db: Db[A]): Db[Throwable \/ A] =
      Db.withDbRead(r => DbValueT[Task, Throwable \/ A](db.run.run(r).run.attempt.map({
        case -\/(e) => DbValue.ok(e.left[A])
        case \/-(v) => v.map(_.right[Throwable])
      })))

    def fail[A](err: Throwable): Db[A] =
      liftTask(Task.fail(err))
  }
}
