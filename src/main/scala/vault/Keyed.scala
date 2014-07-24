package vault

import scalaz._, Scalaz._

case class Keyed[A](id: Long, value: A) {
  def map[B](f: A => B): Keyed[B] =
    Keyed(id, f(value))
}

object Keyed {
  implicit def KeyedFunctor: Functor[Keyed] = new Functor[Keyed] {
    def map[A, B](fa: Keyed[A])(f: A => B) = fa map f
  }

  implicit def KeyedToDb[A: ToDb]: ToDb[Keyed[A]] =
    (ToDb.of[Long] |+| ToDb.of[A]).contramap(k => k.id -> k.value)

  implicit def KeyedFromDb[A: FromDb]: FromDb[Keyed[A]] =
    (FromDb.of[Long] |@| FromDb.of[A])(Keyed.apply)
}
