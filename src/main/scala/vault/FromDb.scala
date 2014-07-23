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

object FromDb extends GeneratedFromDb {
  def of[A: FromDb] =
    implicitly[FromDb[A]]

  def perform[A: FromDb](r: Row): DbValue[A] =
    of[A].perform(r)

  private def run[A: FromDb](n: Int, r: Row): DbValue[(Int, Option[A])] =
    of[A].run(n, r)

  def value[A](a: A): FromDb[A] =
    FromDb((n, _) => DbValue.ok[(Int, Option[A])]((n, Some(a))))

  private def fromDb[A](run: (Int, Row) => DbValue[Option[A]]) =
    FromDb((n, r) => run(n, r).map(v => (n + 1, v)))

  private def fromDbCell[A](run: Cell => DbValue[Option[A]]) =
    fromDb((n, r) => run(r.toCell(n)))

  implicit def FromDbOption[A: FromDb]: FromDb[Option[A]] =
    FromDb((n, r) => run[A](n, r) map {
      case (nn, None) => (nn, Some(None))
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

  implicit def FromDbMonad: Monad[FromDb] = new Monad[FromDb] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: FromDb[A])(f: A => FromDb[B]) = m flatMap f
  }
}
