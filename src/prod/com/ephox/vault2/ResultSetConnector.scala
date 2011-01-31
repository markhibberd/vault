package com.ephox.vault2

import scalaz._
import Scalaz._
import Connector._
import SQLValue._
import java.sql.{PreparedStatement, Connection, ResultSet}

sealed trait ResultSetConnector[A] {
  val rsConnect: ResultSet => Connector[A]

  import ResultSetConnector._

  def apply(rs: ResultSet) =
    rsConnect(rs)

  def <|-(rs: ResultSet) =
    rsConnect(rs)

  def -|>[T](iter: IterV[A, T]): ResultSetConnector[IterV[A, T]] =
      resultSetConnector (rs => {
        def loop(i: IterV[A, T]): Connector[IterV[A, T]] =
          i.fold((a, ip) => i.η[Connector],
                 k => next(rs) flatMap (hasMore =>
                      if (hasMore) rsConnect(rs) flatMap (t => loop(k(IterV.El(t))))
                      else i.η[Connector]))
        loop(iter)
      })

  def -|>>[T](iter: IterV[A, T]) =
    resultSetConnection(r => c => {
          val z = -|>(IterV.head) <|- r
          (z connect c) ∘ (_.run)
      })

  def executeQuery(sql: String): Connector[A] =
    connector(c => withSQLResource(
                     value = c.prepareStatement(sql)
                   , evaluate = (s: PreparedStatement) =>
                       withSQLResource(
                         value    = s.executeQuery
                       , evaluate = (r: ResultSet) => rsConnect(r)(c)
                       )
                   )
    )
}

object ResultSetConnector {
  def resultSetConnector[A](f: ResultSet => Connector[A]): ResultSetConnector[A] = new ResultSetConnector[A] {
    val rsConnect = f
  }

  implicit def ResultSetConnectorFunctor: Functor[ResultSetConnector] = new Functor[ResultSetConnector] {
    def fmap[A, B](k: ResultSetConnector[A], f: A => B) =
      resultSetConnector((r: ResultSet) => k(r) map f)
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
}
