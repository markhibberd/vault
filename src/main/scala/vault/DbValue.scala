package vault

import java.sql.SQLException
import scalaz._, Scalaz._, Free._

sealed trait DbFailure
case class DbNull(column: Int) extends DbFailure
case class DbException(e: SQLException) extends DbFailure

object DbFailure {
  implicit def DbFailureShow: Show[DbFailure] = new Show[DbFailure] {
    override def shows(f: DbFailure) = f match {
      case DbNull(i) =>
        "Unexpected database null value encountered, column(" + i + ")."
      case DbException(e) =>
        "Unexpected database exception encountered: " + e.getMessage
    }
  }
}

sealed trait DbValue[+A] {
  def toEither: DbFailure \/ A = this match {
    case DbOk(a) => a.right[DbFailure]
    case DbErr(e) => e.left[A]
  }

  def fold[X](
    fail: DbFailure => X,
    ok: A => X
  ): X = toEither.fold(fail, ok)

  def toOption =
    toEither.toOption

  def map[B](f: A => B) = this match {
    case DbOk(a) => DbOk(f(a))
    case DbErr(e) => DbErr(e)
  }

  def flatMap[B](f: A => DbValue[B]) = this match {
    case DbOk(a) => f(a)
    case DbErr(e) => DbErr(e)
  }
}

case class DbOk[@specialized(Int, Long, Double) A](a: A) extends DbValue[A]
case class DbErr[A](f: DbFailure) extends DbValue[A]

object DbValue {
  def ok[A](v: A): DbValue[A] =
    DbOk(v)

  def fail[A](f: DbFailure): DbValue[A] =
    DbErr(f)

  def exception[A](e: SQLException): DbValue[A] =
    fail(DbException(e))

  def dbnull[A](column: Int): DbValue[A] =
    fail(DbNull(column))

  def db[A](thunk: => A): DbValue[A] = try {
    thunk.pure[DbValue]
  } catch {
    case (e: SQLException) => DbValue.exception(e)
  }

  implicit def DbValueMonad: Monad[DbValue] = new Monad[DbValue] {
    def point[A](a: => A) = ok(a)
    def bind[A, B](m: DbValue[A])(f: A => DbValue[B]) = m flatMap f
  }
}
