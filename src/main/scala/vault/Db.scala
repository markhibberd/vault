package vault

import scalaz._, Scalaz._, concurrent._, effect._
import java.sql.Connection

case class Db[+A](run: Context[A]) {
  def runLogDb(connection: Connection): IO[(DbHistory, DbValue[A])] = for {
    r <- IO { run(connection).run.run.run }  onException (IO { connection.rollback })
    _ <- IO { connection.commit }
  } yield r

  def runDb(connection: Connection): IO[DbValue[A]] =
    runLogDb(connection).map(_._2)

  def testLogDb(connection: Connection): IO[(DbHistory, DbValue[A])] =
    IO { run(connection).run.run.run } ensuring (IO { connection.rollback })

  def testDb(connection: Connection): IO[DbValue[A]] =
    testLogDb(connection).map(_._2)

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

  def safe[A](f: Connection => DbValue[A]): Db[A] =
    safeWithLog(f.map(v => DbHistory.empty -> v))

  def safeWithLog[A](f: Connection => (DbHistory, DbValue[A])): Db[A] =
    Db.withConnectionX(connection =>
      Task.delay { f(connection) } )

  def liftIO[A](io: IO[A]) =
    Db.withConnectionX(_ =>
      Task.delay { DbHistory.empty-> DbValue.ok(io.unsafePerformIO) } )

  implicit def DbMonad: Monad[Db] = new Monad[Db] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: Db[A])(f: A => Db[B]) = m flatMap f
  }
}
