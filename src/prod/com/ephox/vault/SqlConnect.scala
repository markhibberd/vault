package com.ephox.vault

import scalaz._
import Scalaz._
import java.sql._

sealed trait SqlConnect[L, A] {
  val connect: Connection => SqlValue[L, A]

  def apply(c: Connection) = connect(c)

  def bracket[B, C](after: (=> A) => SqlConnect[L, B], k: (=> A) => SqlConnect[L, C]): SqlConnect[L, C] =
    flatMap (a => try {
      k(a)
    } finally {
      after(a)
    })

  def finaly[B](b: => SqlConnect[L, B]): SqlConnect[L, A] =
    sqlConnect(c => try {
      apply(c)
    } finally {
      b(c)
    })

  def finalyClose: SqlConnect[L, A] =
    finaly(closeSqlConnect)

  /**
   * Commits the connection and if this fails with an exception then rollback the connection.
   *
   * If the failure is an `SqlException` then this is returned in the `SqlValue`, otherwise, the exception is rethrown.
   */
  def commitRollback: SqlConnect[L, A] =
    sqlConnect(c => try {
      val r = connect(c)
      c.commit
      r
    } catch {
      case e: SqlException => {
        c.rollback
        sqlError(e)
      }
      case e => {
        c.rollback
        throw e
      }
    })

  def commitRollbackClose: SqlConnect[L, A] =
    commitRollback.finalyClose

  def map[B](f: A => B): SqlConnect[L, B] =
    sqlConnect(connect(_) map f)

  def flatMap[B](f: A => SqlConnect[L, B]) =
    sqlConnect(c => connect(c) flatMap (f(_) connect c))
}

trait SqlConnects {
  def sqlConnect[L, A](f: Connection => SqlValue[L, A]): SqlConnect[L, A] = /*new SqlConnect[A] {
    val connect = f
  } */
    error("todo")

  def constantSqlConnect[L, A](v: => SqlValue[L, A]): SqlConnect[L, A] =
    // sqlConnect(_ => v)
    error("todo")

  def valueSqlConnect[L, A](f: Connection => A): SqlConnect[L, A] =
    // sqlConnect(f(_).η[SqlValue])
    error("todo")

  def trySqlConnect[L, A](f: Connection => A): SqlConnect[L, A] =
    // sqlConnect(c => trySqlValue(f(c)))
    error("todo")

  def closeSqlConnect[L]: SqlConnect[L, Unit] =
    // trySqlConnect(_.close)
    error("todo")

//  implicit def SqlConnectFunctor: Functor[SqlConnect] = new Functor[SqlConnect] {
//    def fmap[A, B](k: SqlConnect[A], f: A => B) =
//      sqlConnect((c: Connection) => k(c) map f)
//  }
//
//  implicit def SqlConnectPure[M[_]]: Pure[SqlConnect] = new Pure[SqlConnect] {
//    def pure[A](a: => A) =
//      valueSqlConnect(_ => a)
//  }
//
//  implicit def SqlConnectApply[M[_]]: Apply[SqlConnect] = new Apply[SqlConnect] {
//    def apply[A, B](f: SqlConnect[A => B], a: SqlConnect[A]) = {
//      sqlConnect(c => a(c) <*> f(c))
//    }
//  }
//
//  implicit def SqlConnectBind[M[_]]: Bind[SqlConnect] = new Bind[SqlConnect] {
//    def bind[A, B](a: SqlConnect[A], f: A => SqlConnect[B]) =
//      sqlConnect(c => a(c) >>= (a => f(a)(c)))
//  }
}
