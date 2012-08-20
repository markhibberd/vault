package com.ephox.vault

import scalaz._, Scalaz._

sealed trait RowQueryConnect[A] {
  def <|-(sql: Sql): RowConnect[A]

  import RowQueryConnect._

  def map[B](f: A => B): RowQueryConnect[B] =
    rowQueryConnect(s => this <|- s map f)

  def flatMap[B](f: A => RowQueryConnect[B]): RowQueryConnect[B] =
    rowQueryConnect(s => (this <|- s) flatMap (a => f(a) <|- s))
}

object RowQueryConnect extends RowQueryConnects

trait RowQueryConnects {
  def rowQueryConnect[A](f: Sql => RowConnect[A]): RowQueryConnect[A] = new RowQueryConnect[A] {
    def <|-(sql: Sql) =
      f(sql)
  }

  implicit val RowQueryConnectMonad: Monad[RowQueryConnect] = new Monad[RowQueryConnect] {
    def bind[A, B](a: RowQueryConnect[A])(f: A => RowQueryConnect[B]) =
      a flatMap f

    def point[A](a: => A) =
      rowQueryConnect(_ => a.point[RowConnect])
  }
}