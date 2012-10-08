package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import SqlT._

sealed trait XSqlT[F[+_], +A] {
  val run: F[Option[SqlError \/ A]]

  def fold[X](empty: => X, err: SqlError => X, value: A => X)(implicit F: Functor[F]): F[X] =
    F.map(run) {
      case None => empty
      case Some(e) => e.fold(err, value)
    }

  def transformer: OptionT[F, SqlError \/ A] =
    OptionT(run)

  def isError(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists (_.isLeft))

  def isValue(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_ exists (_.isRight))

  def error(implicit F: Functor[F]): OptionT[F, SqlError] =
    OptionT(F.map(run)(_ flatMap (_.swap.toOption)))

  def isEmpty(implicit F: Functor[F]): F[Boolean] =
    F.map(run)(_.isEmpty)

  def map[B](f: A => B)(implicit F: Functor[F]): XSqlT[F, B] =
    XSqlT(F.map(run)(_ map (_ map f)))

  def foreach(f: A => Unit)(implicit E: Each[F]): Unit =
    E.each(run)(_ foreach (_ foreach f))

  def ap[B](f: XSqlT[F, A => B])(implicit F: Apply[F]): XSqlT[F, B] =
    XSqlT(F.apply2(f.run, run)((b, a) => a flatMap (x => b map (y => x flatMap (z => y map (_(z)))))))

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

  def product[B](x: XSqlT[F, B])(implicit F: Apply[F]): XSqlT[F, (A, B)] =
    XSqlT(F.apply2(run, x.run)((a, b) =>
      Apply[Option].apply2(a, b)((aa, bb) =>
        for {
          aaa <- aa
          bbb <- bb
      } yield (aaa, bbb))
    ))

  def ***[B](x: XSqlT[F, B])(implicit F: Apply[F]): XSqlT[F, (A, B)] =
    product(x)

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

  def empty[F[+_], A](implicit F: Pointed[F]): XSqlT[F, A] =
    XSqlT(F.point(None))

  def err[F[+_], A](e: SqlError)(implicit F: Pointed[F]): XSqlT[F, A] =
    XSqlT(F.point(Some(e.left)))

  def value[F[+_], A](a: A)(implicit F: Pointed[F]): XSqlT[F, A] =
    XSqlT(F.point(Some(a.right)))

  object TryNullT {
    def apply[F[+_], A](x: F[() => A])(implicit F: Functor[F]): XSqlT[F, A] =
      XSqlT(F.map(x)(a => try {
        val r = a()
        if(r == null) None else Some(r.right)
      } catch {
        case e: java.sql.SQLException => Some(SqlError.sqlException(e).left)
        case e: java.sql.SQLWarning => Some(SqlError.sqlWarning(e).left)
        case e: java.sql.DataTruncation => Some(SqlError.dataTruncation(e).left)
        case e: java.sql.BatchUpdateException => Some(SqlError.batchUpdateException(e).left)
      }))
  }

  object XTry {
    def apply[A](x: => A): XSql[A] =
      TryNullT[Id, A](() => x)
  }
}

trait XSqlTFunctions extends XSqlTInstances {
  type XSql[+A] =
  XSqlT[Id, A]
}

trait XSqlTInstances0 {
   implicit def xsqlTFunctor[F[+_]](implicit F0: Functor[F]): Functor[({type f[a] = XSqlT[F, a]})#f] = new XSqlTFunctor[F] {
     implicit def F: Functor[F] = F0
   }
 
   implicit val xsqlTMonadTrans: MonadTrans[XSqlT] = new XSqlTMonadTrans {
 
   }
 }
 
 trait XSqlTInstances1 extends XSqlTInstances0 {
   implicit def xsqlTPointed[F[+_]](implicit F0: Pointed[F]): Pointed[({type f[a] = XSqlT[F, a]})#f] = new XSqlTPointed[F] {
     implicit def F: Pointed[F] = F0
   }
 
   implicit val xsqlTHoist: Hoist[SqlT] = new SqlTHoist {
 
   }
 }
 
 trait XSqlTInstances2 extends XSqlTInstances1 {
   implicit def xsqlTApply[F[+_]](implicit F0: Apply[F]): Apply[({type f[a] = XSqlT[F, a]})#f] = new XSqlTApply[F] {
     implicit def F: Apply[F] = F0
   }
 }
 
 trait XSqlTInstances3 extends XSqlTInstances2 {
   implicit def xsqlTApplicative[F[+_]](implicit F0: Applicative[F]): Applicative[({type f[a] = XSqlT[F, a]})#f] = new XSqlTApplicative[F] {
     implicit def F: Applicative[F] = F0
   }
 }
 
 trait XSqlTInstances4 extends XSqlTInstances3 {
   implicit def xsqlTBind[F[+_]](implicit F0: Monad[F]): Bind[({type f[a] = XSqlT[F, a]})#f] = new XSqlTBind[F] {
     implicit def F: Monad[F] = F0
   }
 }
 
 trait XSqlTInstances5 extends XSqlTInstances4 {
   implicit def xsqlTMonad[F[+_]](implicit F0: Monad[F]): Monad[({type f[a] = XSqlT[F, a]})#f] = new XSqlTMonad[F] {
     implicit def F: Monad[F] = F0
   }
 }
 
 trait XSqlTInstances extends XSqlTInstances5
 
 private[sql] trait XSqlTFunctor[F[+_]] extends Functor[({type f[+a] = XSqlT[F, a]})#f] {
   implicit def F: Functor[F]
 
   override def map[A, B](a: XSqlT[F, A])(f: A => B) = a map f
 }
 
 private[sql] trait XSqlTPointed[F[+_]] extends Pointed[({type f[+a] = XSqlT[F, a]})#f] with XSqlTFunctor[F] {
   implicit def F: Pointed[F]
 
   override def point[A](a: => A) = XSqlT(F.point(Some(a.right)))
 }
 
 private[sql] trait XSqlTApply[F[+_]] extends Apply[({type f[+a] = XSqlT[F, a]})#f] with XSqlTFunctor[F] {
   implicit def F: Apply[F]
 
   override def ap[A, B](a: => XSqlT[F, A])(f: => XSqlT[F, A => B]) =
     a ap f
 }
 
 private[sql] trait XSqlTApplicative[F[+_]] extends Applicative[({type f[+a] = XSqlT[F, a]})#f] with XSqlTApply[F] with XSqlTPointed[F] {
   implicit def F: Applicative[F]
 }
 
 private[sql] trait XSqlTBind[F[+_]] extends Bind[({type f[+a] = XSqlT[F, a]})#f] with XSqlTApply[F] {
   implicit def F: Monad[F]
 
   override def bind[A, B](a: XSqlT[F, A])(f: A => XSqlT[F, B]) = a flatMap f
 
 }
 
 private[sql] trait XSqlTMonad[F[+_]] extends Monad[({type f[+a] = XSqlT[F, a]})#f] with XSqlTApplicative[F] with XSqlTBind[F] {
   implicit def F: Monad[F]
 }
 
 private[sql] trait XSqlTMonadTrans extends MonadTrans[XSqlT] {
   override def liftM[G[+_] : Monad, A](a: G[A]): XSqlT[G, A] =
     XSqlT(implicitly[Functor[G]].map(a)(x => Some(x.right)))

 
   implicit def apply[G[+_]](implicit M: Monad[G]) = new XSqlTMonad[G] {
     implicit def F = M
   }
 
 }
 
 private[sql] trait XSqlTHoist extends Hoist[XSqlT] with XSqlTMonadTrans {
   override def hoist[M[+_]: Monad, N[+_]](f: M ~> N): ({type f[x] = XSqlT[M, x]})#f ~> ({type f[x] = XSqlT[N, x]})#f =
     new (({type f[x] = XSqlT[M, x]})#f ~> ({type f[x] = XSqlT[N, x]})#f) {
       def apply[A](x: XSqlT[M, A]) =
         XSqlT(f(x.run))
     }
 }
                    