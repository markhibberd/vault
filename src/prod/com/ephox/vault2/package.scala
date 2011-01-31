package com.ephox

import scalaz._
import Scalaz._
import java.sql.{PreparedStatement, ResultSet, SQLException, Connection}
import vault2.{ResultSetConnector, SQLValue, Connector}

package object vault2 {
  def sqlErr[A](e: SQLException): SQLValue[A] =
    SQLValue.err(e)

  def sqlValue[A](v: A): SQLValue[A] =
    SQLValue.value(v)

  def tryValue[A](a: => A): SQLValue[A] =
    try {
      sqlValue(a)
    } catch {
      case e: SQLException => sqlErr(e)
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

  def valueConnector[A](f: Connection => A): Connector[A] =
    connector(f(_).η[SQLValue])

  def tryConnector[A](f: Connection => A): Connector[A] =
    connector(c => try {
      sqlValue(f(c))
    } catch {
      case e: SQLException => sqlErr(e)
    })

  val close: Connector[Unit] =
    tryConnector(_.close)

  def withPreparedStatement[A](k: PreparedStatement => A): String => Connector[A] =
    sql => tryConnector(
      c => {
        val st = c.prepareStatement(sql)

        try {
          k(st)
        } finally {
          st.close
        }
      })

  def withResultSet[A](k: ResultSet => A): ResultSet => Connector[A] =
    r =>
      tryConnector(_ => try {
        k(r)
      } finally {
        r.close
      })

  def withExecuteQuery[A]: String => (ResultSet => A) => Connector[A] =
    sql => k =>
      withPreparedStatement(_.executeQuery)(sql) flatMap (withResultSet(k))

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