package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait ISqlT[F[+_], +A] {
  val run: F[Incompatibility \/ SqlError \/ A]

  def transformer: EitherT[F, Incompatibility \/ SqlError, A] =
    EitherT(run)

  def switch(implicit F: Functor[F]): F[Incompatibility \/ (SqlError \/ A)] =
    F.map(run)(_.fold(_.map(_.left), _.right.right))

  def transformerSwitch(implicit F: Functor[F]): EitherT[F, Incompatibility, SqlError \/ A] =
    EitherT(switch)

  def isError(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(e => ~e exists (_.isRight))

  def isIncompatibility(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(e => ~e exists (_.isLeft))

  def isValue(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isRight)

  def error(implicit F: Functor[F]): OptionT[F, SqlError] =
    OptionT(F.map(run)(_.swap.toOption flatMap (_.toOption)))

  def incompatibility(implicit F: Functor[F]): OptionT[F, Incompatibility] =
    OptionT(F.map(run)(_.swap.toOption flatMap (_.swap.toOption)))

  def map[B](f: A => B)(implicit F: Functor[F]): ISqlT[F, B] =
    ISqlT(F.map(run)(_ map f))

  def foreach(f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach f)

  def ap[B](f: ISqlT[F, A => B])(implicit F: Apply[F]): ISqlT[F, B] =
    ISqlT(F(run, f.run)((a, b) => a flatMap (x => b map (_(x)))))

  def flatMap[B](f: A => ISqlT[F, B])(implicit F: Monad[F]): ISqlT[F, B] =
    ISqlT(F.bind(run)(a => a.fold(
      e => F.point(e.left)
    , f(_).run
    )))

  def traverse[G[+_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[ISqlT[F, B]] =
    G.map(F.traverse(run)(_ traverse f))(ISqlT(_))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[Incompatibility \/ SqlError \/ A, Z](run, z)((a, b) => a.foldRight(b)(f))

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

  def toEither(implicit F: Functor[F]): F[Either[Either[Incompatibility, SqlError], A]] =
    F.map(run)(_.toEither.left.map(_.toEither))

  def toValidation(implicit F: Functor[F]): F[Validation[Validation[Incompatibility, SqlError], A]] =
    F.map(run)(_.validation ~ (_ map (_.validation)))

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    F.map(run)(_ getOrElse default)

  def |[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    getOrElse(default)

  def orElse[AA >: A](x: => ISqlT[F, AA])(implicit F: Bind[F]): ISqlT[F, AA] =
    ISqlT(F.bind(run)(a => a.fold(
      _ => x.run
    , _ => run
    )))

  def |||[AA >: A](x: => ISqlT[F, AA])(implicit F: Bind[F]): ISqlT[F, AA] =
    orElse(x)

  def unary_~(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(switch)(_.toOption))

  def unary_-[AA >: A](implicit F: F[Incompatibility \/ (SqlError \/ AA)] =:= Id[Incompatibility \/ (SqlError \/ AA)], T: Functor[F]): SqlT[({type λ[+α] = Incompatibility \/ α})#λ, AA] =
    SqlT[({type λ[+α] = Incompatibility \/ α})#λ, AA](switch: Incompatibility \/ (SqlError \/ AA))

}

object ISqlT extends ISqlTFunctions {
  def apply[F[+_], A](x: F[Incompatibility \/ SqlError \/ A]): ISqlT[F, A] =
    new ISqlT[F, A] {
      val run = x
    }
}

trait ISqlTFunctions {
  type ISql[+A] =
  ISqlT[Id, A]
}
