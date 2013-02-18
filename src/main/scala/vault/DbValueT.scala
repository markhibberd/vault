package vault

import java.sql.SQLException
import scalaz._, Scalaz._, Free._

case class DbValueT[F[+_], +A](run: F[DbFailure \/ A]) {
  type DbValueTF[+A] = DbValueT[F, A]
  type FreeDbT[+A] = Free[DbValueTF, A]

  def fold[X](fail: DbFailure => X, ok: A => X)(implicit F: Functor[F]): F[X] =
    run.map(_.fold(fail, ok))

  def toOption(implicit F: Functor[F]) =
    run.map(_.toOption)

  def map[B](f: A => B)(implicit F: Functor[F]): DbValueT[F, B] =
    DbValueT(run.map(_.map(f)))

  def flatMap[B](f: A => DbValueT[F, B])(implicit F: Monad[F]): DbValueT[F, B] =
    DbValueT(run.flatMap(_.fold(e => F.point(e.left), f(_).run)))

  def free(implicit M: Monad[F]): FreeDbT[A] =
    Suspend[DbValueTF, A](map(Return(_)))

// Compile error because there is a diverging implicit (No Monad for F for DbValueTMonad).
// This results in a SOE against 2.9.2. The correct code is above.

//  def freeScalaFailedImplicitResolutionResultsInStackOverflow(implicit M: Functor[F]): FreeDbT[A] =
//    Suspend[DbValueTF, A](map(Return(_)))

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
    DbValueT[F, A](thunk.right.pure[F])
  } catch {
    case (e: SQLException) => exception[F, A](e)
  }

  def freedb[F[+_]: Monad, A](thunk: => A) =
    db[F, A](thunk).free

// Compile error because F can not be inferenced on call to db.
// This results in a SOE against 2.9.2. The correct code is above.

//  def freedbScalaInferenceFailResultsInStackOverflow[F[+_]: Monad, A](thunk: => A) =
//    db(thunk).free

  implicit def DbValueTMonad[F[+_]: Monad]: Monad[({type f[+a] = DbValueT[F, a]})#f] = new Monad[({type f[+a] = DbValueT[F, a]})#f] {
    def point[A](a: => A) = ok(a)
    def bind[A, B](m: DbValueT[F, A])(f: A => DbValueT[F, B]) = m flatMap f
  }
}
