package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql._

sealed trait SQLConnect[A] {
  val connect: Connection => SQLValue[A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => SQLConnect[B], k: (=> A) => SQLConnect[C]): SQLConnect[C] =
    flatMap (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => SQLConnect[B]): SQLConnect[A] =
    sqlConnect(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: SQLConnect[A] =
    finaly(closeSqlConnect)

  /**
   * Commits the connection and if this fails with an exception then rollback the connection.
   *
   * If the failure is an `SQLException` then this is returned in the `SQLValue`, otherwise, the exception is rethrown.
   */
  def commitRollback: SQLConnect[A] =
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

  def commitRollbackClose: SQLConnect[A] =
    commitRollback.finalyClose

  def map[B](f: A => B): SQLConnect[B] =
    sqlConnect(connect(_) map f)

  def flatMap[B](f: A => SQLConnect[B]) =
    sqlConnect(c => connect(c) flatMap (f(_) connect c))
}

trait SqlConnects {
  def sqlConnect[A](f: Connection => SQLValue[A]): SQLConnect[A] = new SQLConnect[A] {
    val connect = f
  }

  def constantSqlConnect[A](v: => SQLValue[A]): SQLConnect[A] =
    sqlConnect(_ => v)

  def valueSqlConnect[A](f: Connection => A): SQLConnect[A] =
    sqlConnect(f(_).η[SQLValue])

  def trySqlConnect[A](f: Connection => A): SQLConnect[A] =
    sqlConnect(c => trySQLValue(f(c)))

  val closeSqlConnect: SQLConnect[Unit] =
    trySqlConnect(_.close)

  implicit def SQLConnectFunctor: Functor[SQLConnect] = new Functor[SQLConnect] {
    def fmap[A, B](k: SQLConnect[A], f: A => B) =
      sqlConnect((c: Connection) => k(c) map f)
  }

  implicit def SQLConnectPure[M[_]]: Pure[SQLConnect] = new Pure[SQLConnect] {
    def pure[A](a: => A) =
      valueSqlConnect(_ => a)
  }

  implicit def SQLConnectApply[M[_]]: Apply[SQLConnect] = new Apply[SQLConnect] {
    def apply[A, B](f: SQLConnect[A => B], a: SQLConnect[A]) = {
      sqlConnect(c => a(c) <*> f(c))
    }
  }

  implicit def SQLConnectBind[M[_]]: Bind[SQLConnect] = new Bind[SQLConnect] {
    def bind[A, B](a: SQLConnect[A], f: A => SQLConnect[B]) =
      sqlConnect(c => a(c) >>= (a => f(a)(c)))
  }
}
