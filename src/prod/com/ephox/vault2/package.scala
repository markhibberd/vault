package com.ephox

import scalaz._
import Scalaz._
import java.sql.{ResultSet, SQLException, Connection}
import vault2.{StringQuery, ResultSetConnector, SQLValue, Connector}

package object vault2 {
  implicit def StringStringQuery(s: String): StringQuery =
    StringQuery.stringQuery(s)

  implicit def StringQueryString(sql: StringQuery): String =
    sql.sql

  def sqlErr[A](e: SQLException): SQLValue[A] =
    SQLValue.err(e)

  def sqlValue[A](v: A): SQLValue[A] =
    SQLValue.value(v)

  def tryValue[A](a: => A): SQLValue[A] =
    try {
      sqlValue(a)
    } catch {
      case e: SQLException => sqlErr(e)
      case e               => throw e
    }

  def withSQLResource[T, R](
                          value: => T
                        , evaluate: T => SQLValue[R]
                        , whenClosing: Throwable => Unit = _ => ()
                        )(implicit r: Resource[T]): SQLValue[R] =
    withResource(value, evaluate, {
      case e: SQLException => sqlErr(e)
      case e               => throw e
    }, whenClosing)

  def connector[A](f: Connection => SQLValue[A]): Connector[A] =
    Connector.connector(f)

  def constantConnector[A](v: => SQLValue[A]): Connector[A] =
    connector(_ => v)

  def valueConnector[A](f: Connection => A): Connector[A] =
    connector(f(_).η[SQLValue])

  def tryConnector[A](f: Connection => A): Connector[A] =
    connector(c => tryValue(f(c)))

  val close: Connector[Unit] =
    tryConnector(_.close)

  def resultSetConnector[A](f: ResultSet => Connector[A]): ResultSetConnector[A] =
    ResultSetConnector.resultSetConnector(f)

  def tryResultSetConnector[A](f: ResultSet => Connection => A): ResultSetConnector[A] =
    resultSetConnector((r: ResultSet) => tryConnector(c => f(r)(c)))

  def constantResultSetConnector[A](c: => Connector[A]): ResultSetConnector[A] =
    resultSetConnector(_ => c)

  def rResultSetConnector[A](f: ResultSet => A): ResultSetConnector[A] =
    resultSetConnector(f(_).η[Connector])

  def resultSetConnection[A](f: ResultSet => Connection => SQLValue[A]): ResultSetConnector[A] =
    resultSetConnector(r => connector(f(r)))

  // WARNING: side-effects on rs
  val next = resultSetConnector((rs: ResultSet) =>
    rs.next.η[Connector])

}