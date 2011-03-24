package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql._

sealed trait SqlConnect[A] {
  val connect: Connection => SQLValue[A]

  def apply(c: Connection) = connect(c)

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
   * If the failure is an `SQLException` then this is returned in the `SQLValue`, otherwise, the exception is rethrown.
   */
  def commitRollback: SqlConnect[A] =
    sqlConnect(c => try {
      val r = connect(c)
      c.commit
      r
    } catch {
      case e: SQLException => {
        c.rollback
        sqlError(e)
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
}

trait SqlConnects {
  def sqlConnect[A](f: Connection => SQLValue[A]): SqlConnect[A] = new SqlConnect[A] {
    val connect = f
  }

  def constantSqlConnect[A](v: => SQLValue[A]): SqlConnect[A] =
    sqlConnect(_ => v)

  def valueSqlConnect[A](f: Connection => A): SqlConnect[A] =
    sqlConnect(f(_).η[SQLValue])

  def trySqlConnect[A](f: Connection => A): SqlConnect[A] =
    sqlConnect(c => trySQLValue(f(c)))

  val closeSqlConnect: SqlConnect[Unit] =
    trySqlConnect(_.close)

  implicit def SqlConnectFunctor: Functor[SqlConnect] = new Functor[SqlConnect] {
    def fmap[A, B](k: SqlConnect[A], f: A => B) =
      sqlConnect((c: Connection) => k(c) map f)
  }

  implicit def SqlConnectPure[M[_]]: Pure[SqlConnect] = new Pure[SqlConnect] {
    def pure[A](a: => A) =
      valueSqlConnect(_ => a)
  }

  implicit def SqlConnectApply[M[_]]: Apply[SqlConnect] = new Apply[SqlConnect] {
    def apply[A, B](f: SqlConnect[A => B], a: SqlConnect[A]) = {
      sqlConnect(c => a(c) <*> f(c))
    }
  }

  implicit def SqlConnectBind[M[_]]: Bind[SqlConnect] = new Bind[SqlConnect] {
    def bind[A, B](a: SqlConnect[A], f: A => SqlConnect[B]) =
      sqlConnect(c => a(c) >>= (a => f(a)(c)))
  }
}
