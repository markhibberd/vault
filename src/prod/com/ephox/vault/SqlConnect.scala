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
  def sqlConnect[L, A](f: Connection => SqlValue[L, A]): SqlConnect[L, A] = new SqlConnect[L, A] {
    val connect = f
  }

  def constantSqlConnect[L, A](v: => SqlValue[L, A]): SqlConnect[L, A] =
    sqlConnect(_ => v)

  def valueSqlConnect[L, A](f: Connection => A): SqlConnect[L, A] =
    sqlConnect(f(_).η[({type λ[α]= SqlValue[L, α]})#λ])

  def trySqlConnect[L, A](f: Connection => A): SqlConnect[L, A] =
    sqlConnect(c => trySqlValue(f(c)))

  def closeSqlConnect[L]: SqlConnect[L, Unit] =
    trySqlConnect(_.close)

  implicit def SqlConnectFunctor[L]: Functor[({type λ[α]= SqlConnect[L, α]})#λ] = new Functor[({type λ[α]= SqlConnect[L, α]})#λ] {
    def fmap[A, B](k: SqlConnect[L, A], f: A => B) =
      k map f
  }

  implicit def SqlConnectPure[L, M[_]]: Pure[({type λ[α]= SqlConnect[L, α]})#λ] = new Pure[({type λ[α]= SqlConnect[L, α]})#λ] {
    def pure[A](a: => A) =
      valueSqlConnect(_ => a)
  }

  implicit def SqlConnectApply[L, M[_]]: Apply[({type λ[α]= SqlConnect[L, α]})#λ] = new Apply[({type λ[α]= SqlConnect[L, α]})#λ] {
    def apply[A, B](f: SqlConnect[L, A => B], a: SqlConnect[L, A]) = {
      sqlConnect(c => {
        val fc = f(c)
        a(c) <*> fc
      })
    }
  }

  implicit def SqlConnectBind[L, M[_]]: Bind[({type λ[α]= SqlConnect[L, α]})#λ] = new Bind[({type λ[α]= SqlConnect[L, α]})#λ] {
    def bind[A, B](a: SqlConnect[L, A], f: A => SqlConnect[L, B]) =
      sqlConnect(c => a(c) >>= (a => f(a)(c)))
  }
}
