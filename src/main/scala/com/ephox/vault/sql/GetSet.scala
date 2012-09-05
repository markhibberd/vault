package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import XSqlT._
import SqlT._

sealed trait GetSet[A] {
  val column: Column
  val get: XSql[A]
  val set: A => Sql[Unit]

  def :=(a: A): Sql[Unit] =
    set(a)

  def run: Sql[Unit] =
    SqlT.Sql(get.run match {
      case None => ().right
      case Some(t) => t.flatMap(set(_).run)
    })

  def unary_! : XSql[A] =
    get

  def xmap[B](f: A => B, g: B => A): GetSet[B] =
    new GetSet[B] {
      val column = GetSet.this.column
      val get = GetSet.this.get map f
      val set = (b: B) => GetSet.this.set(g(b))
    }

}

object GetSet {
  def apply[A](c: Column, g: => A, s: A => Unit): GetSet[A] =
    new GetSet[A] {
      val column = c
      val get = TryNull(g)
      val set = (a: A) => Try(s(a))
    }
}