package vault

import java.sql.SQLException
import scalaz._, Scalaz._, Free._

case class DbValueT[F[+_], +A](run: F[DbFailure \/ A]) {
  type DbValueTF[+Z] = DbValueT[F, Z]
  type FreeDbT[+A] = Free[DbValueTF, A]

  def fold[X](fail: DbFailure => X, ok: A => X)(implicit F: Functor[F]): F[X] =
    run.map(_.fold(fail, ok))

  def toOption(implicit F: Functor[F]) =
    run.map(_.toOption)

  def map[B](f: A => B)(implicit F: Functor[F]): DbValueT[F, B] =
    DbValueT(run.map(_.map(f)))

  def flatMap[B](f: A => DbValueT[F, B])(implicit F: Monad[F]): DbValueT[F, B] =
    DbValueT(run.flatMap(_.fold(e => F.point(e.left), f(_).run)))
  def free(implicit F: Functor[F]): FreeDbT[A] =
    Suspend(map(Return(_)))
}

object DbValueT {
  def ok[F[+_]: Monad, A](v: A): DbValueT[F, A] =
    DbValueT(v.right.pure[F])

  def fail[F[+_]: Monad, A](f: DbFailure): DbValueT[F, A] =
    DbValueT(f.left.pure[F])

  def exception[F[+_]: Monad, A](e: SQLException): DbValueT[F, A] =
    fail(DbException(e))

  def dbnull[F[+_]: Monad, A]: DbValueT[F, A] =
    fail(DbNull())

  def db[F[+_]: Monad, A](thunk: => A): DbValueT[F, A] = try {
    val a: A = thunk
    val e: DbFailure \/ A = a.right
    val f: F[DbFailure \/ A] = e.pure[F]
    val d: DbValueT[F, A] = DbValueT[F, A](f)
    d
  } catch {
    case (e: SQLException) => exception[F, A](e)
  }

//  def freedb[A](thunk: => A): Free[DbValue, A] =
//    db(thunk).free

  implicit def DbValueMonadT[F[+_]: Monad]: Monad[({type f[+a] = DbValueT[F, a]})#f] = new Monad[({type f[+a] = DbValueT[F, a]})#f] {
    def point[A](a: => A) = ok(a)
    def bind[A, B](m: DbValueT[F, A])(f: A => DbValueT[F, B]) = m flatMap f
  }
}
