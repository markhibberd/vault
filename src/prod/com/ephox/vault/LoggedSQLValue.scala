package com.ephox.vault

import scalaz._
import Scalaz._

import LoggedSQLValue._
import java.sql.SQLException

sealed trait LoggedSQLValue[A] {
  val log: LOG
  def fold[X](ex: SQLException => X, value: A => X): X
}

object LoggedSQLValue {
  type LOG = List[String] // todo use better data structure

  // Pure, Functor, Apply, Bind, Each, Index, Length, Foldable, Traverse, Plus, Empty, Show, Equal, Order, Zero
}

trait LoggedSQLValues {
  def loggedSqlValue[A](l: LOG, v: A): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val log = l
    def fold[X](ex: SQLException => X, value: A => X) = value(v)
  }

  def loggedSqlException[A](l: LOG, e: SQLException): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val log = l
    def fold[X](ex: SQLException => X, value: A => X) = ex(e)
  }
}