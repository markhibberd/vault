package vault

import scalaz._, Scalaz._
import java.sql.Connection

case class Db[+A](runDb: ReaderWriterDbValue[A]) {
  def apply(connection: Connection): DbValue[(Log, A)] =
    runDb(connection).run

  def map[B](f: A => B): Db[B] =
    flatMap(a => Db.value(f(a)))

  def flatMap[B](f: A => Db[B]) =
    Db(connection => apply(connection).fold(
      f => WriterT.put(DbValue.fail(f))(Vector[String]()),
      {
        case (l, a) => f(a).apply(connection).fold(
          f => WriterT.put(DbValue.fail(f))(l),
          { case (ll, b) => WriterT.put(DbValue.ok(b))(l ++ ll) }
        )
      }
    ))
}

object Db {
  def value[A](a: A): Db[A] =
    apply(_ => a.pure[WriterDbValue])

  def apply[A](run: Connection => WriterDbValue[A]): Db[A] =
    Db(Kleisli(run))

  implicit def DbMonad: Monad[Db] = new Monad[Db] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: Db[A])(f: A => Db[B]) = m flatMap f
  }
}
