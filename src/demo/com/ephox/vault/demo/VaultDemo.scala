package com.ephox.vault.demo

import scalaz._
import Scalaz._
import com.ephox.vault._
import Vault._

object VaultDemo {
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
      def connection = com.ephox.vault.Connect.hsqlfile(args(0), args(1), args(2)).nu

      // get the first of the query results for a Person
      val firstPerson = IterV.peek[Person]

      // compare Person by name for equality
      val equalByName = ((_:Person).name).equaling

      // Get a List of lists of people grouped by name.
      val groupedByName = IterV.repeat[Person, List[Person], List](IterV.groupBy(equalByName))

      // combine the two person accessors
      val combine = for {
        h <- firstPerson
        i <- groupedByName
      } yield (h, i)

      // initialise data
      setupData commitRollback connection printStackTraceOr (n => println(n + " rows affected"))

      // get result and close connection
      val combinedPerson = PersonRowAccess -||> combine <|- "SELECT * FROM PERSON" finalyClose connection

      // print the result
      combinedPerson.println
    }
  }
}
