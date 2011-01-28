package com.ephox.vault2.demo

import scalaz._
import Scalaz._
import com.ephox.vault2.ResultSetConnector._
import com.ephox.vault2.Connector._
import java.sql.Connection
import com.ephox.vault2.ResultSetConnector

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

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

      // Initialise data
      setupData(connection)

      val c = connection

      val z = PersonResultSetConnector -|> IterV.head <|- executeQuery("SELECT * FROM PERSON").finalyClose

      val firstPerson = (z connect c) ∘ (_.run)

      println(firstPerson fold (
            e => e
          , p => p
          ))

      println("Connection is " + (if(c.isClosed) "" else " not ") + "closed")
    }
  }
}
