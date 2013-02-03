package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait SqlT[F[+_], +A] {
  val run: F[SqlError \/ A]

  def fold[X](err: SqlError => X, value: A => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.fold(err, value))

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
    SqlT(F.apply2(f.run, run)((b, a) => a flatMap (x => b map (_(x)))))

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

  def product[B](x: SqlT[F, B])(implicit F: Apply[F]): SqlT[F, (A, B)] =
    SqlT(F.apply2(run, x.run)((a, b) =>
      for {
          aa <- a
          bb <- b
      } yield (aa, bb))
    )

  def ***[B](x: SqlT[F, B])(implicit F: Apply[F]): SqlT[F, (A, B)] =
    product(x)

  def unary_~(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(run)(Some(_)))

  def ![B](f: A => Incompatibility \/ B)(implicit F: Functor[F]): ISqlT[F, B] =
    ISqlT(F.map(run)(_.fold(_.right.left, f(_) ~ (_ map (_.left)))))
}

object SqlT extends SqlTFunctions with SqlTInstances {
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

trait SqlTInstances0 {
  implicit def sqlTFunctor[F[+_]](implicit F0: Functor[F]): Functor[({type f[a] = SqlT[F, a]})#f] = new SqlTFunctor[F] {
    implicit def F: Functor[F] = F0
  }

  implicit val sqlTMonadTrans: MonadTrans[SqlT] = new SqlTMonadTrans {

  }
}

trait SqlTInstances1 extends SqlTInstances0 {
  implicit def sqlTPointed[F[+_]](implicit F0: Pointed[F]): Pointed[({type f[a] = SqlT[F, a]})#f] = new SqlTPointed[F] {
    implicit def F: Pointed[F] = F0
  }

  implicit val sqlTHoist: Hoist[SqlT] = new SqlTHoist {

  }
}

trait SqlTInstances2 extends SqlTInstances1 {
  implicit def sqlTApply[F[+_]](implicit F0: Apply[F]): Apply[({type f[a] = SqlT[F, a]})#f] = new SqlTApply[F] {
    implicit def F: Apply[F] = F0
  }
}

trait SqlTInstances3 extends SqlTInstances2 {
  implicit def sqlTApplicative[F[+_]](implicit F0: Applicative[F]): Applicative[({type f[a] = SqlT[F, a]})#f] = new SqlTApplicative[F] {
    implicit def F: Applicative[F] = F0
  }
}

trait SqlTInstances4 extends SqlTInstances3 {
  implicit def sqlTBind[F[+_]](implicit F0: Monad[F]): Bind[({type f[a] = SqlT[F, a]})#f] = new SqlTBind[F] {
    implicit def F: Monad[F] = F0
  }
}

trait SqlTInstances5 extends SqlTInstances4 {
  implicit def sqlTMonad[F[+_]](implicit F0: Monad[F]): Monad[({type f[a] = SqlT[F, a]})#f] = new SqlTMonad[F] {
    implicit def F: Monad[F] = F0
  }
}

trait SqlTInstances extends SqlTInstances5

private[sql] trait SqlTFunctor[F[+_]] extends Functor[({type f[+a] = SqlT[F, a]})#f] {
  implicit def F: Functor[F]

  override def map[A, B](a: SqlT[F, A])(f: A => B) = a map f
}

private[sql] trait SqlTPointed[F[+_]] extends Pointed[({type f[+a] = SqlT[F, a]})#f] with SqlTFunctor[F] {
  implicit def F: Pointed[F]

  override def point[A](a: => A) = SqlT(F.point(a.right))
}

private[sql] trait SqlTApply[F[+_]] extends Apply[({type f[+a] = SqlT[F, a]})#f] with SqlTFunctor[F] {
  implicit def F: Apply[F]

  override def ap[A, B](a: => SqlT[F, A])(f: => SqlT[F, A => B]) =
    a ap f
}

private[sql] trait SqlTApplicative[F[+_]] extends Applicative[({type f[+a] = SqlT[F, a]})#f] with SqlTApply[F] with SqlTPointed[F] {
  implicit def F: Applicative[F]
}

private[sql] trait SqlTBind[F[+_]] extends Bind[({type f[+a] = SqlT[F, a]})#f] with SqlTApply[F] {
  implicit def F: Monad[F]

  override def bind[A, B](a: SqlT[F, A])(f: A => SqlT[F, B]) = a flatMap f

}

private[sql] trait SqlTMonad[F[+_]] extends Monad[({type f[+a] = SqlT[F, a]})#f] with SqlTApplicative[F] with SqlTBind[F] {
  implicit def F: Monad[F]
}

private[sql] trait SqlTMonadTrans extends MonadTrans[SqlT] {
  override def liftM[G[+_] : Monad, A](a: G[A]): SqlT[G, A] =
    SqlT(implicitly[Functor[G]].map(a)(_.right))

  implicit def apply[G[+_]](implicit M: Monad[G]) = new SqlTMonad[G] {
    implicit def F = M
  }

}

private[sql] trait SqlTHoist extends Hoist[SqlT] with SqlTMonadTrans {
  override def hoist[M[+_]: Monad, N[+_]](f: M ~> N): ({type f[x] = SqlT[M, x]})#f ~> ({type f[x] = SqlT[N, x]})#f =
    new (({type f[x] = SqlT[M, x]})#f ~> ({type f[x] = SqlT[N, x]})#f) {
      def apply[A](x: SqlT[M, A]) =
        SqlT(f(x.run))
    }
}
