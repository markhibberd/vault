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

  implicit def ResultSetConnectorFunctor: Functor[ResultSetConnector] = new Functor[ResultSetConnector] {
    def fmap[A, B](k: ResultSetConnector[A], f: A => B) =
      error("")
  }

  implicit def ResultSetConnectorPure[M[_]]: Pure[ResultSetConnector] = new Pure[ResultSetConnector] {
    def pure[A](a: => A) =
      resultSetConnector(_ => a.η[Connector])
  }

  implicit def ResultSetConnectorApply[M[_]]: Apply[ResultSetConnector] = new Apply[ResultSetConnector] {
    def apply[A, B](f: ResultSetConnector[A => B], a: ResultSetConnector[A]) =
      resultSetConnector(c => a(c) <*> f(c))
  }

  implicit def ResultSetConnectorBind[M[_]]: Bind[ResultSetConnector] = new Bind[ResultSetConnector] {
    def bind[A, B](a: ResultSetConnector[A], f: A => ResultSetConnector[B]) =
      resultSetConnector(c => a(c) >>= (a => f(a)(c)))
  }

  def constantResultSetConnector[A](c: => Connector[A]): ResultSetConnector[A] =
    resultSetConnector(_ => c)

  def rResultSetConnector[A](f: ResultSet => A): ResultSetConnector[A] =
    resultSetConnector(f(_).η[Connector])

  // WARNING: side-effects on rs
  val next = resultSetConnector((rs: ResultSet) =>
    rs.next.η[Connector])
}