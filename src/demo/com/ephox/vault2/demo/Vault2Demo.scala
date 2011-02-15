package com.ephox.vault2.demo

import scalaz._
import Scalaz._
import com.ephox.vault2._

object Vault2Demo {
  case class Person(name: String, age: Int)

  object Person {
    implicit val ShowPerson: Show[Person] = showA
  }

  val data =
    List(
          "Bob" -> 45
        , "Bob" -> 54
        , "Mary" -> 78
        , "Fred" -> 99
        , "Jack" -> 9999
        , "Mark" -> 9999
        , "Mark" -> 9999
        ) map { case (name, age) => Person(name, age) }

  val PersonRowAccess =
    for {
      name <- stringIndex(2)
      age  <- intIndex(3)
    } yield Person(name, age)

  def setupData =
    for {
      a <- "DROP TABLE IF EXISTS PERSON".executeUpdate
      b <- "CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER)".executeUpdate
      p <- "INSERT INTO PERSON(name, age) VALUES (?,?)" prepareStatement
             (s => s.foreachStatement(data, (p: Person) => p match {
               case Person(name, age) => {
                 s.set(stringType(name), intType(age))
               }
             }))
    } yield a + b + p

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      // use file-based database
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

      // get the head of the query results for a Person
      val row = PersonRowAccess -||> IterV.head

      // Get a List of lists of people grouped by name.
      val groupedByName = PersonRowAccess -||> IterV.repeat[Person, List[Person], List](IterV.groupBy((p1: Person, p2: Person) => p1.name == p2.name))

      // initialise data
      setupData commitRollbackClose connection printStackTraceOr (n => println(n + " rows affected"))

      // get result and close connection
      val firstPerson = (row <|- "SELECT * FROM PERSON") finalyClose connection

      // get result and close connection
      val adjacentPerson = (groupedByName <|- "SELECT * FROM PERSON") finalyClose connection

      // print the first person result
      firstPerson.println

      // print the first pair of person result
      adjacentPerson.println
    }
  }
}
