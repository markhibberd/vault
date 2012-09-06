package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import XSqlT._
import SqlT._

sealed trait XGetSet[A] {
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

  def xmap[B](f: A => B, g: B => A): XGetSet[B] =
    new XGetSet[B] {
      val column = XGetSet.this.column
      val get = XGetSet.this.get map f
      val set = (b: B) => XGetSet.this.set(g(b))
    }

}

object XGetSet {
  def apply[A](c: Column, g: => A, s: A => Unit): XGetSet[A] =
    new XGetSet[A] {
      val column = c
      val get = XTry(g)
      val set = (a: A) => Try(s(a))
    }
}