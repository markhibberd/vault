package com.ephox.vault2.demo

import scalaz._
import Scalaz._
import com.ephox.vault2._

object Vault2Demo {
  case class Person(name: String, age: Int)

  val data =
    List(
          "Bob" -> 45
        , "Mary" -> 78
        , "Fred" -> 99
        , "Jack" -> 9999
        ) map { case (name, age) => Person(name, age) }

  val PersonResultSetConnector =
    rResultSetConnector (rs => {
      val name = rs.getString(2)
      val age = rs.getInt(3)
      Person(name, age)
    })

  def setupData =
    for {
      a <- "DROP TABLE IF EXISTS PERSON".executeUpdate
      b <- "CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER)".executeUpdate
      q <- "INSERT INTO PERSON(name, age) VALUES (?,?)" prepareStatement (s =>
             constantConnector(data.foldLeftM(0) {
               case (n, Person(name, age)) => {
                 s.setString(1, name)
                 s.setInt(2, age)
                 tryValue(s.executeUpdate) ∘ (n+)
               }
             }))
    } yield a + b + q

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      // use file-based database
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

      // get the head of the query results
      val personConnector = PersonResultSetConnector -|>> IterV.head

      // select all persons
      val personConnect = "SELECT * FROM PERSON" executeQuery personConnector

      // initialise data
      setupData commitRollbackClose connection printStackTraceOr (n => println(n + " rows affected"))

      // get result and close connection
      val firstPerson = personConnect finalyClose connection

      // print the result
      firstPerson printStackTraceOr (p =>
        println(p match {
          case Some(p) => p.toString
          case None    => "No person"
        }))
    }
  }
}
