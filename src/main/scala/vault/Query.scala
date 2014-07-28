package vault

import scalaz.stream._
import scalaz._, Scalaz._, effect._, Effect._, concurrent._

case class Query[A](query: String, binding: A)(implicit A: ToDb[A]) {
  def bind[B: ToDb](b: B)(implicit ev: A =:= Unit): Query[B] =
    Query(query, b)

  def list[B: FromDb]: Db[List[B]] =
    Execute.list[A, B](query, binding)

  def get[B: FromDb]: Db[Option[B]] =
    Execute.get[A, B](query, binding)

  def update: Db[Int] =
    Execute.update[A](query, binding)

  def execute: Db[Boolean] =
    Execute.execute[A](query, binding)

  def process[B: FromDb]: Process[Db, B] =
    Execute.process[A, B](query, binding)
}
