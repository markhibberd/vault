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
}

object DbValue {
  def ok[A](v: A): DbValue[A] =
    DbValue(v.right)

  def fail[A](f: DbFailure): DbValue[A] =
    DbValue(f.left)

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
