package vault

import java.sql.SQLException
import scalaz._, Scalaz._, Free._

trait DbFailure
case class DbNull() extends DbFailure
case class DbException(e: SQLException) extends DbFailure

case class DbValue[+A](toEither: DbFailure \/ A) {
  def fold[X](
    fail: DbFailure => X,
    ok: A => X
  ): X = toEither.fold(fail, ok)

  def toOption =
    toEither.toOption

  def map[B](f: A => B) =
    DbValue(toEither.map(f))

  def flatMap[B](f: A => DbValue[B]) =
    DbValue(toEither.flatMap(a => f(a).toEither))

  def free: FreeDb[A] =
    Suspend(map(Return(_)))
}

object DbValue {
  def ok[A](v: A): DbValue[A] =
    DbValue(v.right)

  def fail[A](f: DbFailure): DbValue[A] =
    DbValue(f.left)

  def exception[A](e: SQLException): DbValue[A] =
    fail(DbException(e))

  def dbnull[A]: DbValue[A] =
    fail(DbNull())

  def db[A](thunk: => A): DbValue[A] = try {
    thunk.pure[DbValue]
  } catch {
    case (e: SQLException) => DbValue.exception(e)
  }

  def freedb[A](thunk: => A): Free[DbValue, A] =
    db(thunk).free

  implicit def DbValueMonad: Monad[DbValue] = new Monad[DbValue] {
    def point[A](a: => A) = ok(a)
    def bind[A, B](m: DbValue[A])(f: A => DbValue[B]) = m flatMap f
  }
}
