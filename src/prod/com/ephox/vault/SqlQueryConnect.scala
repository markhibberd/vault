package com.ephox.vault

import scalaz._, Scalaz._

sealed trait SqlQueryConnect[A] {
  def <|-(sql: Query): SqlConnect[A]

  import SqlQueryConnect._

  def map[B](f: A => B): SqlQueryConnect[B] =
    sqlQueryConnect(s => this <|- s map f)

  def flatMap[B](f: A => SqlQueryConnect[B]): SqlQueryConnect[B] =
    sqlQueryConnect(s => (this <|- s) flatMap (a => f(a) <|- s))
}

object SqlQueryConnect extends SqlQueryConnects

trait SqlQueryConnects {
  def sqlQueryConnect[A](f: Query => SqlConnect[A]): SqlQueryConnect[A] = new SqlQueryConnect[A] {
    def <|-(sql: Query) =
      f(sql)
  }

  implicit val SqlQueryConnectFunctor: Functor[SqlQueryConnect] = new Functor[SqlQueryConnect] {
    def fmap[A, B](k: SqlQueryConnect[A], f: A => B) =
      k map f
  }

  implicit val SqlQueryConnectPure: Pure[SqlQueryConnect] = new Pure[SqlQueryConnect] {
    def pure[A](a: => A) =
      sqlQueryConnect(_ => a.η[SqlConnect])
  }

  implicit val SqlQueryConnectBind: Bind[SqlQueryConnect] = new Bind[SqlQueryConnect] {
    def bind[A, B](a: SqlQueryConnect[A], f: A => SqlQueryConnect[B]) =
      a flatMap f
  }
}
