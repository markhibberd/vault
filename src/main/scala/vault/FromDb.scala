package vault

import java.sql.ResultSet
import scalaz._, Scalaz._

case class FromDb[+A](private val run: (Int, Row) => DbValue[(Int, Option[A])]) {
  def map[B](f: A => B): FromDb[B] =
    flatMap(a => FromDb.value(f(a)))

  def flatMap[B](f: A => FromDb[B]): FromDb[B] =
    FromDb((n, r) => run(n, r).fold(DbValue.fail, {
      case (inc, None) => DbValue.ok((n + inc, none))
      case (inc, Some(a)) => f(a).run(n + inc, r)
    }))

  def perform(r: Row): DbValue[A] =
    run(1, r).fold(DbValue.fail, {
      case (_, None) => DbValue.dbnull[A]
      case (_, Some(a)) => DbValue.ok(a)
    })
}

object FromDb {
  def get[A: FromDb] =
    implicitly[FromDb[A]]

  def value[A](a: A): FromDb[A] =
    FromDb((_, _) => DbValue.ok[(Int, Option[A])]((0, Some(a))))

  private def fromDb[A](run: (Int, Row) => DbValue[Option[A]]) =
    FromDb((n, r) => run(n, r).map(v => (1, v)))

  private def fromDbCell[A](run: Cell => DbValue[Option[A]]) =
    fromDb((n, r) => run(r.toCell(n)))

  implicit def FromDbInt: FromDb[Int] =
    fromDbCell(_.int)

  implicit def FromDbLong: FromDb[Long] =
    fromDbCell(_.long)

  implicit def FromDbString: FromDb[String] =
    fromDbCell(_.string)

  implicit def FromDbBoolean: FromDb[Boolean] =
    fromDbCell(_.boolean)

  implicit def FromDbTuple2[A: FromDb, B: FromDb]: FromDb[(A, B)] =
    (get[A] |@| get[B])((_, _))

  implicit def FromDbTuple3[A: FromDb, B: FromDb, C: FromDb]: FromDb[(A, B, C)] =
    (get[A] |@| get[B] |@| get[C])((_, _, _))

  implicit def FromDbMonad: Monad[FromDb] = new Monad[FromDb] {
    def point[A](a: => A) = value(a)
    def bind[A, B](m: FromDb[A])(f: A => FromDb[B]) = m flatMap f
  }
}
