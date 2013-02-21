package vault

import java.sql.ResultSet
import scalaz._, Scalaz._

case class FromDb[+A](private val run: (Int, Row) => DbValue[(Int, Option[A])]) {
  def map[B](f: A => B): FromDb[B] =
    flatMap(a => FromDb.value(f(a)))

  def flatMap[B](f: A => FromDb[B]): FromDb[B] =
    FromDb((n, r) => run(n, r).fold(DbValue.fail, {
      case (nn, None) => DbValue.ok((nn, none))
      case (nn, Some(a)) => f(a).run(nn, r)
    }))

  def perform(r: Row): DbValue[A] =
    run(1, r).fold(DbValue.fail, {
      case (n, None) => DbValue.dbnull[A](n - 1)
      case (_, Some(a)) => DbValue.ok(a)
    })
}

object FromDb {
  def get[A: FromDb] =
    implicitly[FromDb[A]]

  def value[A](a: A): FromDb[A] =
    FromDb((n, _) => DbValue.ok[(Int, Option[A])]((n, Some(a))))

  private def fromDb[A](run: (Int, Row) => DbValue[Option[A]]) =
    FromDb((n, r) => run(n, r).map(v => (n + 1, v)))

  private def fromDbCell[A](run: Cell => DbValue[Option[A]]) =
    fromDb((n, r) => run(r.toCell(n)))

  implicit def FromDbOption[A: FromDb]: FromDb[Option[A]] =
    FromDb((n, r) => get[A].run(n, r) map {
      case (nn, None) => (nn, None)
      case (nn, Some(a)) => (nn, Some(Some(a)))
    })

  implicit def FromDbInt: FromDb[Int] =
    fromDbCell(_.int)

  implicit def FromDbLong: FromDb[Long] =
    fromDbCell(_.long)

  implicit def FromDbString: FromDb[String] =
    fromDbCell(_.string)

  implicit def FromDbBoolean: FromDb[Boolean] =
    fromDbCell(_.boolean)

  implicit def FromDbTuple2[A: FromDb, B: FromDb]: FromDb[(A, B)] = for {
    a <- get[A]
    b <- get[B]
  } yield (a, b)

  implicit def FromDbTuple3[A: FromDb, B: FromDb, C: FromDb]: FromDb[(A, B, C)] = for {
    a <- get[A]
    b <- get[B]
    c <- get[C]
  } yield (a, b, c)

  implicit def FromDbMonad: Monad[FromDb] = new Monad[FromDb] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: FromDb[A])(f: A => FromDb[B]) = m flatMap f
  }
}
