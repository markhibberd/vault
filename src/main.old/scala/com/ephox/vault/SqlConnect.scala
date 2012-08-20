package com.ephox.vault

import scalaz._, Scalaz._
import SqlValue._
import SqlExceptionContext._
import java.sql._

sealed trait SqlConnect[A] {
  val connect: Connection => SqlValue[A]

  import SqlConnect._
  import RowConnect._

  def apply(c: Connection) = connect(c)

  def executeOrDie(c: Connection) = connect(c).getOrDie

  def bracket[B, C](after: (=> A) => SqlConnect[B], k: (=> A) => SqlConnect[C]): SqlConnect[C] =
    flatMap (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => SqlConnect[B]): SqlConnect[A] =
    sqlConnect(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: SqlConnect[A] =
    finaly(closeSqlConnect)

  /**
   * Commits the connection and if this fails with an exception then rollback the connection.
   *
   * If the failure is an `SqlException` then this is returned in the `SqlValue`, otherwise, the exception is rethrown.
   */
  def commitRollback: SqlConnect[A] =
    sqlConnect(c => try {
      val r = connect(c)
      c.commit
      r
    } catch {
      case e: SqlException => {
        c.rollback
        sqlError(sqlExceptionContext(e))
      }
      case e => {
        c.rollback
        throw e
      }
    })

  def commitRollbackClose: SqlConnect[A] =
    commitRollback.finalyClose

  def map[B](f: A => B): SqlConnect[B] =
    sqlConnect(connect(_) map f)

  def flatMap[B](f: A => SqlConnect[B]) =
    sqlConnect(c => connect(c) flatMap (f(_) connect c))

  def toRowConnect: RowConnect[A] =
    rowConnect(connect(_).toRowValue)

  def toKleisli: Kleisli[SqlValue, Connection, A] =
    Kleisli(connect)

  // Unsafe function
  // Prints the given argument during execution of the connection value.
  def trace(a: A)(implicit s: Show[A]): SqlConnect[A] =
    sqlConnect(c => {
      a.println
      connect(c)
    })

}

object SqlConnect extends SqlConnects

trait SqlConnects {
  def sqlConnect[A](f: Connection => SqlValue[A]): SqlConnect[A] = new SqlConnect[A] {
    val connect = f
  }

  def constantSqlConnect[A](v: => SqlValue[A]): SqlConnect[A] =
    sqlConnect(_ => v)

  def valueSqlConnect[A](f: Connection => A): SqlConnect[A] =
    sqlConnect(f(_).point[SqlValue])

  def trySqlConnect[A](f: Connection => A): SqlConnect[A] =
    sqlConnect(c => trySqlValue(f(c)))

  def closeSqlConnect: SqlConnect[Unit] =
    trySqlConnect(_.close)

  def kleisliSqlConnect[A](k: Kleisli[SqlValue, Connection, A]): SqlConnect[A] =
    sqlConnect(k(_))

  def foldTraverseSqlConnect[T[_]: Traverse, A, B](w: T[A], g: A => SqlConnect[B]): SqlConnect[T[B]] =
    kleisliSqlConnect(w.traverseKTrampoline[SqlValue, Connection, B](a => g(a).toKleisli))

  implicit val SqlConnectMonad: Monad[SqlConnect] =
    new Monad[SqlConnect]{
    def bind[A, B](a: SqlConnect[A])(f: A => SqlConnect[B]) =
      sqlConnect(c => a(c) flatMap  (a => f(a)(c)))

    def point[A](a: => A) =
      valueSqlConnect(_ => a)
  }
}
