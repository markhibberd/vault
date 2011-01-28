package com.ephox.vault2.demo

import scalaz._
import Scalaz._
import com.ephox.vault2.ResultSetConnector._
import com.ephox.vault2.Connector._
import com.ephox.vault2.SQLValue._
import java.sql.{SQLException, ResultSet, PreparedStatement, Connection}
import com.ephox.vault2.{SQLValue, ResultSetConnector}

object Vault2Demo {
  case class Person(name: String, age: Int)

  val data =
    List(
          "Bob" -> 45
        , "Mary" -> 78
        , "Fred" -> 99
        ) map { case (name, age) => Person(name, age) }

  val PersonResultSetConnector =
    rResultSetConnector (rs => {
      val name = rs.getString(2)
      val age = rs.getInt(3)
      Person(name, age)
    })

  def setupData(c: Connection) = {
    try {
      val n = c.createStatement.executeUpdate("CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER)")
      val p = c.prepareStatement("INSERT INTO PERSON(name, age) VALUES (?,?)")

      data foreach { case per@Person(name, age) => {
        p.setString(1, name)
        p.setInt(2, age)
        val r = p.executeUpdate
        println("Inserted " + per + " with result " + r)
      }}
    } finally {
      c.close
    }
  }

  // todo ResultSetConnector[A] => String => Connector[A]
  def executeQuery[A](sql: String, withResultSet: ResultSet => Connection => SQLValue[A]): Connection => SQLValue[A] =
    (c: Connection) =>
        withSQLResource(
            value    = c.prepareStatement(sql)
          , evaluate = (s: PreparedStatement) =>
              withSQLResource(
                value    = s.executeQuery
              , evaluate = (r: ResultSet) => withResultSet(r)(c)
              )
          )


  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

      // Initialise data
      setupData(connection)

      val firstPerson = withSQLResource(
        value = connection
      , evaluate =
            executeQuery(
              sql           = "SELECT * FROM PERSON"
            , withResultSet = (r: ResultSet) => (c: Connection) => {
                  val z = PersonResultSetConnector -|> IterV.head <|- r
                  (z connect c) ∘ (_.run)
              }
            )
      )

      println(firstPerson fold (
                            e => e
                          , p => p
                          ))
    }
  }
}
