package com.ephox.vault2

import java.sql.PreparedStatement
import scalaz._
import Scalaz._

sealed trait PreparedStatementW {
  val s: PreparedStatement

  def tryExecuteUpdate: SQLValue[Int] =
    tryValue(s.executeUpdate)

  def executeStatements[T[_], A](as: T[A], k: A => Connector[Unit])(implicit f: Foldable[T]): Connector[Int] =
    connector(c => as.foldLeftM(0) {
       case (n, a) => {
         k(a)(c)
         s.tryExecuteUpdate ∘ (n+)
       }
     })

  def foreachStatement[T[_], A](as: T[A], k: A => Unit)(implicit f: Foldable[T]): Connector[Int] =
    executeStatements(as, (a: A) => k(a).η[Connector])
}

object PreparedStatementW {
  implicit def PreparedStatementPreparedStatementW(t: PreparedStatement): PreparedStatementW = new PreparedStatementW {
    val s = t
  }

  implicit def PreparedStatementWPreparedStatement(t: PreparedStatementW): PreparedStatement =
    t.s
}
