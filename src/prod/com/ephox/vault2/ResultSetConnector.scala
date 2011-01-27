package com.ephox.vault2

import scalaz._
import Scalaz._
import Connector._
import SQLValue._
import java.sql.ResultSet

sealed trait ResultSetConnector[A] {
  val rsConnect: ResultSet => Connector[A]

  import ResultSetConnector._

  def apply(rs: ResultSet) =
    rsConnect(rs)

  def <|-(ct: Connector[ResultSet]): Connector[A] =
    connector(c => ct(c) flatMap (apply(_) connect c))

  def -|>[T](iter: IterV[A, T]): ResultSetConnector[IterV[A, T]] =
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

  def constantResultSetConnector[A](c: => Connector[A]): ResultSetConnector[A] =
    resultSetConnector(_ => c)

  def rResultSetConnector[A](f: ResultSet => A): ResultSetConnector[A] =
    resultSetConnector(f(_).η[Connector])

  // WARNING: side-effects on rs
  val next = resultSetConnector((rs: ResultSet) =>
    rs.next.η[Connector])
}