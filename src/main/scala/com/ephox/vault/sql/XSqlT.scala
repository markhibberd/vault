package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import SqlT._

sealed trait XSqlT[F[+_], +A] {
  val run: F[Option[SqlError \/ A]]

  def transformer: OptionT[F, SqlError \/ A] =
    OptionT(run)

  def isError(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists (_.isLeft))

  def isValue(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists (_.isRight))

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isEmpty)

  def map[B](f: A => B)(implicit F: Functor[F]): XSqlT[F, B] =
    XSqlT(F.map(run)(_ map (_ map f)))

  def foreach(f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach (_ foreach f))

  def ap[B](f: XSqlT[F, A => B])(implicit F: Apply[F]): XSqlT[F, B] =
    XSqlT(F(run, f.run)((a, b) => a flatMap (x => b map (y => x flatMap (z => y map (_(z)))))))

  def flatMap[B](f: A => XSqlT[F, B])(implicit F: Monad[F]): XSqlT[F, B] =
    XSqlT(F.bind(run) {
      case None => F.point(None)
      case Some(r) => r.fold(
        e => F.point(Some(e.left))
      , a => f(a).run
      )
    })

  def traverse[G[+_], B](f: A => G[B])(implicit F: Traverse[F], G: Applicative[G]): G[XSqlT[F, B]] =
    G.map(F.traverse(run)(_ traverse (_ traverse f)))(XSqlT(_))

  def foldRight[Z](z: => Z)(f: (A, => Z) => Z)(implicit F: Foldable[F]): Z =
    F.foldRight[Option[SqlError \/ A], Z](run, z)((a, b) => a match {
      case None => b
      case Some(r) => r.foldRight(b)(f)
    })

  def exists(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists (_ exists f))

  def forall(f: A => Boolean)(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ forall (_ forall f))

  def toList(implicit F: Functor[F]): F[List[A]] =
    F.map(run)(x => x.toList flatMap (_.toList))

  def toStream(implicit F: Functor[F]): F[Stream[A]] =
    F.map(run)(x => x.toStream flatMap (_.toStream))

  def toOption(implicit F: Functor[F]): OptionT[F, A] =
    OptionT(F.map(run)(_ flatMap (_.toOption)))

  def getOrElse[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    F.map(run) {
      case None => default
      case Some(r) => r getOrElse default
    }

  def |[AA >: A](default: => AA)(implicit F: Functor[F]): F[AA] =
    getOrElse(default)

  def orElse[AA >: A](x: => XSqlT[F, AA])(implicit F: Bind[F]): XSqlT[F, AA] =
    XSqlT(F.bind(run) {
      case None => x.run
      case Some(r) => r.fold(_ => x.run, _ => run)
    })

  def |||[AA >: A](x: => XSqlT[F, AA])(implicit F: Bind[F]): XSqlT[F, AA] =
    orElse(x)

  def -<:(e: => SqlError)(implicit F: Functor[F]): SqlT[F, A] =
    SqlT(F.map(run)(_ getOrElse e.left))

  def :>-[AA >: A](a: => AA)(implicit F: Functor[F]): SqlT[F, AA] =
    SqlT(F.map(run)(_ getOrElse a.right))

  def unary_-[AA >: A](implicit F: F[Option[SqlError \/ AA]] =:= Id[Option[SqlError \/ AA]]): SqlT[Option, AA] =
    SqlT(run: Option[SqlError \/ AA])

}

object XSqlT extends XSqlTFunctions {
  def apply[F[+_], A](x: F[Option[SqlError \/ A]]): XSqlT[F, A] =
    new XSqlT[F, A] {
      val run = x
    }
}

trait XSqlTFunctions {
  type XSql[+A] =
  XSqlT[Id, A]
}
