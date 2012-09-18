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
