package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import SqlT._

sealed trait GetSet[A] {
  val column: Column
  val get: Sql[A]
  val set: A => Sql[Unit]

  def :=(a: A): Sql[Unit] =
    set(a)

  def run: Sql[Unit] =
    get flatMap set

  def unary_! : Sql[A] =
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
      val get = Try(g)
      val set = (a: A) => Try(s(a))
    }
}