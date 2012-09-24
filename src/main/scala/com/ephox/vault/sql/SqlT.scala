package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait SqlT[F[+_], +A] {
  val run: F[SqlError \/ A]

  def transformer: EitherT[F, SqlError, A] =
    EitherT(run)

  sealed trait SwitchingJSqlT[X] {
    def v: X

    def <<?:(err: => X)(implicit F: Functor[F]): F[X] =
      F.map(SqlT.this.run)(_.fold(_ => err, _ => v))
  }

  def :?>>[X](value: => X): SwitchingJSqlT[X] =
    new SwitchingJSqlT[X] {
      def v = value
    }

  def isError(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isLeft)

  def isValue(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  def error(implicit F: Functor[F]): OptionT[F, SqlError] =
    OptionT(F.map(run)(_.swap.toOption))

  def map[B](f: A => B)(implicit F: Functor[F]): SqlT[F, B] =
    SqlT(F.map(run)(_ map f))

  def foreach(f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach f)

  def ap[B](f: SqlT[F, A => B])(implicit F: Apply[F]): SqlT[F, B] =
    SqlT(F(run, f.run)((a, b) => a flatMap (x => b map (_(x)))))

  def flatMap[B](f: A => SqlT[F, B])(implicit F: Monad[F]): SqlT[F, B] =
    SqlT(F.bind(run)(_.fold(e => F.point(e.left), f(_).run)))

  def traverse[G[+_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[SqlT[F, B]] =
    G.map(F.traverse(run)(_ traverse f))(SqlT(_))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[SqlError \/ A, Z](run, z)((a, b) => a.foldRight(b)(f))

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists f)

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall f)

  def toList(implicit F: Functor[F]): F[List[A]] =
    F.map(run)(_.toList)

  def toStream(implicit F: Functor[F]): F[Stream[A]] =
    F.map(run)(_.toStream)

  def toOption(implicit F: Functor[F]): OptionT[F, A] =
    transformer.toOption

  def toEither(implicit F: Functor[F]): F[Either[SqlError, A]] =
    F.map(run)(_.toEither)

  def toValidation(implicit F: Functor[F]): F[Validation[SqlError, A]] =
    F.map(run)(_.validation)

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    F.map(run)(_ getOrElse default)

  def |[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    getOrElse(default)

  def valueOr[AA >: A](x: SqlError => AA)(implicit F: Functor[F]): F[AA] =
      F.map(run)(_ valueOr x)

  def orElse[AA >: A](x: => SqlT[F, AA])(implicit F: Bind[F]): SqlT[F, AA] =
    SqlT(F.bind(run)(_.fold(_ => x.run, _ => run)))

  def |||[AA >: A](x: => SqlT[F, AA])(implicit F: Bind[F]): SqlT[F, AA] =
    orElse(x)

  def unary_~(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(run)(Some(_)))

  def ![B](f: A => Incompatibility \/ B)(implicit F: Functor[F]): ISqlT[F, B] =
    ISqlT(F.map(run)(_.fold(_.right.left, f(_) ~ (_ map (_.left)))))
}

object SqlT extends SqlTFunctions {
  def apply[F[+_], A](x: F[SqlError \/ A]): SqlT[F, A] =
    new SqlT[F, A] {
      val run = x
    }
}

trait SqlTFunctions {
  type Sql[+A] =
  SqlT[Id, A]

  object Error {
    def apply[F[+_], A](x: F[SqlError])(implicit F: Functor[F]): SqlT[F, A] =
      SqlT(F.map(x)(_.left))
  }

  object Value {
    def apply[F[+_], A](x: F[A])(implicit F: Functor[F]): SqlT[F, A] =
      SqlT(F.map(x)(_.right))
  }

  object TryT {
    def apply[F[+_], A](x: F[() => A])(implicit F: Functor[F]): SqlT[F, A] =
      SqlT(F.map(x)(a => try {
        a().right
      } catch {
        case e: java.sql.SQLException => SqlError.sqlException(e).left
        case e: java.sql.SQLWarning => SqlError.sqlWarning(e).left
        case e: java.sql.DataTruncation => SqlError.dataTruncation(e).left
        case e: java.sql.BatchUpdateException => SqlError.batchUpdateException(e).left
      }))
  }

  object Sql {
    def apply[A](x: SqlError \/ A): Sql[A] =
      SqlT[Id, A](x)
  }

  object Try {
    def apply[A](x: => A): Sql[A] =
      TryT[Id, A](() => x)
  }

  object JSql {
    def apply[A](a: SqlError \/ A): Sql[A] =
      SqlT[Id, A](a)
  }
}
