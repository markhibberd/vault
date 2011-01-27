package com.ephox.vault2

import scalaz._
import Scalaz._
import SQLValue._
import java.sql.ResultSet

sealed trait ResultSetConnector[A] {
  val rsConnect: ResultSet => Connector[A]

  import ResultSetConnector._

  def apply(rs: ResultSet) =
    rsConnect(rs)

  def enumerate[T](iter: IterV[A, T]): ResultSetConnector[IterV[A, T]] =
      resultSetConnector (rs => {
        def loop(i: IterV[A, T]): Connector[IterV[A, T]] =
          i.fold((a, ip) => i.η[Connector],
                 k => next(rs) flatMap (hasMore =>
                      if (hasMore) rsConnect(rs) flatMap (t => loop(k(IterV.El(t))))
                      else i.η[Connector]))
        loop(iter)
      })
}

object ResultSetConnector {
  def resultSetConnector[A](f: ResultSet => Connector[A]): ResultSetConnector[A] = new ResultSetConnector[A] {
    val rsConnect = f
  }

  // WARNING: side-effects on rs
  val next = resultSetConnector((rs: ResultSet) =>
    rs.next.η[Connector])
}