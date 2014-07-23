package vault

import java.sql.SQLException
import scalaz._, Scalaz._, Free._

case class DbValueT[F[+_], +A](run: F[DbValue[A]]) {
  type DbValueTF[+A] = DbValueT[F, A]

  def fold[X](fail: DbFailure => X, ok: A => X)(implicit F: Functor[F]): F[X] =
    run.map(_.fold(fail, ok))

  def toOption(implicit F: Functor[F]) =
    run.map(_.toOption)

  def map[B](f: A => B)(implicit F: Functor[F]): DbValueT[F, B] =
    DbValueT(run.map(_.map(f)))

  def flatMap[B](f: A => DbValueT[F, B])(implicit F: Monad[F]): DbValueT[F, B] =
    DbValueT(run.flatMap(_.fold(e => F.point(DbValue.fail(e)), f(_).run)))
}

object DbValueT {
  def ok[F[+_]: Monad, A](v: A): DbValueT[F, A] =
    value(DbValue.ok(v))

  def value[F[+_]: Monad, A](v: DbValue[A]): DbValueT[F, A] =
    DbValueT(v.pure[F])

  def fail[F[+_]: Monad, A](f: DbFailure): DbValueT[F, A] =
    DbValueT(DbValue.fail(f).pure[F])

  def exception[F[+_]: Monad, A](e: SQLException): DbValueT[F, A] =
    fail(DbException(e))

  def dbnull[F[+_]: Monad, A](column: Int): DbValueT[F, A] =
    fail(DbNull(column))

  def db[F[+_]: Monad, A](thunk: => A): DbValueT[F, A] = try {
    ok(thunk)
  } catch {
    case (e: SQLException) => exception[F, A](e)
  }

  implicit def DbValueTMonad[F[+_]: Monad]: Monad[({type f[+a] = DbValueT[F, a]})#f] = new Monad[({type f[+a] = DbValueT[F, a]})#f] {
    def point[A](a: => A) = ok(a)
    def bind[A, B](m: DbValueT[F, A])(f: A => DbValueT[F, B]) = m flatMap f
  }
}
