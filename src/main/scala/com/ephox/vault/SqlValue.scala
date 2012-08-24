package com.ephox
package vault

import SqlValue._
import scalaz._, Scalaz._

sealed trait SqlValue[+L, +A] {
  val log: Log[L]
  val value: SqlExceptionContext \/ A
}

object SqlValue extends SqlValueFunctions {
  def apply[L, A](l: Log[L], v: SqlExceptionContext \/ A): SqlValue[L, A] =
    new SqlValue[L, A] {
      val log = l
      val value = v
    }
}

trait SqlValueFunctions {
  type Log[+L] =
  Vector[L]

  def sqlValueLogL[L, A]: SqlValue[L, A] @> Log[L] =
    Lens(v => Store(SqlValue(_, v.value), v.log))

  def sqlValueOrL[L, A]: SqlValue[L, A] @> (SqlExceptionContext \/ A) =
    Lens(v => Store(SqlValue(v.log, _), v.value))

  def sqlValueContextPL[L, A]: SqlValue[L, A] @?> SqlExceptionContext =
    ~sqlValueOrL >=> PLensT.leftPLens

  def sqlValuePL[L, A]: SqlValue[L, A] @?> A =
    ~sqlValueOrL >=> PLensT.rightPLens
}
