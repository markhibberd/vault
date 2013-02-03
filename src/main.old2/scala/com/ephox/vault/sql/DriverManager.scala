package com.ephox
package vault
package sql

import scalaz._, Scalaz._
import SqlT._
import collection.JavaConversions._
import java.sql.{DriverManager => D}

object DriverManager {
  sealed trait GetConnection {
    val url: String
    val properties: Option[java.util.Properties \/ (String, String)]
  }

  object GetConnection {
    def apply(s: String): GetConnection =
      new GetConnection {
        val url = s
        val properties = None
      }

    object Properties {
      def apply(s: String, p: java.util.Properties): GetConnection =
        new GetConnection {
          val url = s
          val properties = Some(p.left)
        }
    }

    object UserPass {
      def apply(s: String, u: String, p: String): GetConnection =
        new GetConnection {
          val url: String = s
          val properties = Some((u, p).right)
        }
    }
  }

  def deregisterDriver(driver: Driver): Sql[Unit] =
    Try(D.deregisterDriver(driver.x))

  def connection(x: GetConnection): Sql[Connection] =
    Try(Connection(x.properties match {
      case None => D.getConnection(x.url)
      case Some(e) => e.fold(
        p => D.getConnection(x.url, p)
      , c => D.getConnection(x.url, c._1, c._2)
      )
    }))

  def driver(url: String): Sql[Driver] =
    Try(Driver(D.getDriver(url)))

  def drivers: Sql[Iterator[Driver]] =
    Try(JEnumerationWrapper(D.getDrivers) map (Driver(_)))

  def loginTimeout: Int =
    D.getLoginTimeout

  def logWriter: java.io.PrintWriter =
    D.getLogWriter

  def println[A: Show](a: A): Unit =
    D.println(implicitly[Show[A]].shows(a))

  def registerDriver(driver: Driver): Sql[Unit] =
    Try(D.registerDriver(driver.x))

  def setLoginTimeout(seconds: Int): Unit =
    D.setLoginTimeout(seconds)

  def setLogWriter(out: java.io.PrintWriter): Unit =
    D.setLogWriter(out)
}
