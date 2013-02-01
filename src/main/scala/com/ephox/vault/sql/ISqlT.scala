package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait ISqlT[F[+_], +A] {
  val run: F[Incompatibility \/ SqlError \/ A]

  def fold[X](incompatibility: Incompatibility => X, err: SqlError => X, value: A => X)(implicit F: Functor[F]): F[X] =
    F.map(run)(_.fold(_.fold(incompatibility, err), value))

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
    ISqlT(F.apply2(f.run, run)((b, a) => a flatMap (x => b map (_(x)))))

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

  def product[B](x: ISqlT[F, B])(implicit F: Apply[F]): ISqlT[F, (A, B)] =
    ISqlT(F.apply2(run, x.run)((a, b) =>
      for {
          aa <- a
          bb <- b
      } yield (aa, bb))
    )

  def ***[B](x: ISqlT[F, B])(implicit F: Apply[F]): ISqlT[F, (A, B)] =
    product(x)

  def unary_~(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(switch)(_.toOption))

  def -<:(e: => SqlError)(implicit F: Functor[F]): SqlT[F, A] =
    e -<: ~this

  def :>-[AA >: A](a: => AA)(implicit F: Functor[F]): SqlT[F, AA] =
    ~this :>- a

  def unary_-[AA >: A](implicit F: F[Incompatibility \/ (SqlError \/ AA)] =:= Id[Incompatibility \/ (SqlError \/ AA)], T: Functor[F]): SqlT[({type λ[+α] = Incompatibility \/ α})#λ, AA] =
    SqlT[({type λ[+α] = Incompatibility \/ α})#λ, AA](switch: Incompatibility \/ (SqlError \/ AA))

}

object ISqlT extends ISqlTFunctions {
  def apply[F[+_], A](x: F[Incompatibility \/ SqlError \/ A]): ISqlT[F, A] =
    new ISqlT[F, A] {
      val run = x
    }
}

trait ISqlTFunctions extends ISqlTInstances {
  type ISql[+A] =
  ISqlT[Id, A]
}

trait ISqlTInstances0 {
   implicit def isqlTFunctor[F[+_]](implicit F0: Functor[F]): Functor[({type f[a] = ISqlT[F, a]})#f] = new ISqlTFunctor[F] {
     implicit def F: Functor[F] = F0
   }

   implicit val isqlTMonadTrans: MonadTrans[ISqlT] = new ISqlTMonadTrans {

   }
 }

 trait ISqlTInstances1 extends ISqlTInstances0 {
   implicit def isqlTPointed[F[+_]](implicit F0: Pointed[F]): Pointed[({type f[a] = ISqlT[F, a]})#f] = new ISqlTPointed[F] {
     implicit def F: Pointed[F] = F0
   }

   implicit val isqlTHoist: Hoist[SqlT] = new SqlTHoist {

   }
 }

 trait ISqlTInstances2 extends ISqlTInstances1 {
   implicit def isqlTApply[F[+_]](implicit F0: Apply[F]): Apply[({type f[a] = ISqlT[F, a]})#f] = new ISqlTApply[F] {
     implicit def F: Apply[F] = F0
   }
 }

 trait ISqlTInstances3 extends ISqlTInstances2 {
   implicit def isqlTApplicative[F[+_]](implicit F0: Applicative[F]): Applicative[({type f[a] = ISqlT[F, a]})#f] = new ISqlTApplicative[F] {
     implicit def F: Applicative[F] = F0
   }
 }

 trait ISqlTInstances4 extends ISqlTInstances3 {
   implicit def isqlTBind[F[+_]](implicit F0: Monad[F]): Bind[({type f[a] = ISqlT[F, a]})#f] = new ISqlTBind[F] {
     implicit def F: Monad[F] = F0
   }
 }

 trait ISqlTInstances5 extends ISqlTInstances4 {
   implicit def isqlTMonad[F[+_]](implicit F0: Monad[F]): Monad[({type f[a] = ISqlT[F, a]})#f] = new ISqlTMonad[F] {
     implicit def F: Monad[F] = F0
   }
 }

 trait ISqlTInstances extends ISqlTInstances5

 private[sql] trait ISqlTFunctor[F[+_]] extends Functor[({type f[+a] = ISqlT[F, a]})#f] {
   implicit def F: Functor[F]

   override def map[A, B](a: ISqlT[F, A])(f: A => B) = a map f
 }

 private[sql] trait ISqlTPointed[F[+_]] extends Pointed[({type f[+a] = ISqlT[F, a]})#f] with ISqlTFunctor[F] {
   implicit def F: Pointed[F]

   override def point[A](a: => A) = ISqlT(F.point(a.right))
 }

 private[sql] trait ISqlTApply[F[+_]] extends Apply[({type f[+a] = ISqlT[F, a]})#f] with ISqlTFunctor[F] {
   implicit def F: Apply[F]

   override def ap[A, B](a: => ISqlT[F, A])(f: => ISqlT[F, A => B]) =
     a ap f
 }

 private[sql] trait ISqlTApplicative[F[+_]] extends Applicative[({type f[+a] = ISqlT[F, a]})#f] with ISqlTApply[F] with ISqlTPointed[F] {
   implicit def F: Applicative[F]
 }

 private[sql] trait ISqlTBind[F[+_]] extends Bind[({type f[+a] = ISqlT[F, a]})#f] with ISqlTApply[F] {
   implicit def F: Monad[F]

   override def bind[A, B](a: ISqlT[F, A])(f: A => ISqlT[F, B]) = a flatMap f

 }

 private[sql] trait ISqlTMonad[F[+_]] extends Monad[({type f[+a] = ISqlT[F, a]})#f] with ISqlTApplicative[F] with ISqlTBind[F] {
   implicit def F: Monad[F]
 }

 private[sql] trait ISqlTMonadTrans extends MonadTrans[ISqlT] {
   override def liftM[G[+_] : Monad, A](a: G[A]): ISqlT[G, A] =
     ISqlT(implicitly[Functor[G]].map(a)(_.right))


   implicit def apply[G[+_]](implicit M: Monad[G]) = new ISqlTMonad[G] {
     implicit def F = M
   }

 }

 private[sql] trait ISqlTHoist extends Hoist[ISqlT] with ISqlTMonadTrans {
   override def hoist[M[+_]: Monad, N[+_]](f: M ~> N): ({type f[x] = ISqlT[M, x]})#f ~> ({type f[x] = ISqlT[N, x]})#f =
     new (({type f[x] = ISqlT[M, x]})#f ~> ({type f[x] = ISqlT[N, x]})#f) {
       def apply[A](x: ISqlT[M, A]) =
         ISqlT(f(x.run))
     }
 }
