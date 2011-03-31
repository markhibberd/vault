package com.ephox.vault.demo

import scalaz._, Scalaz._
import com.ephox.vault._, Vault._

object VaultDemo {
  case class Person(name: String, age: Int, address: PossiblyNull[String])

  object Person {
    implicit val ShowPerson: Show[Person] = showA
  }

  val PersonSql =
    for {
      name    <- stringIndex(2).unifyNull
      age     <- intIndex(3).unifyNull
      address <- stringIndex(4).possiblyNull
    } yield Person(name, age, address)

  val data =
    List(
          "Bob"  -> 45   -> notNull("61 Street")
        , "Bob"  -> 54   -> isNull[String]
        , "Mary" -> 78   -> notNull("87 Street")
        , "Fred" -> 99   -> isNull[String]
        , "Jack" -> 9999 -> isNull[String]
        , "Mark" -> 9999 -> notNull("98 Other Street")
        , "Mark" -> 9999 -> isNull[String]
        ) map {
      case ((name, age), address) => Person(name, age, address)
    }

  def setupData =
    for {
      a <- "DROP TABLE IF EXISTS PERSON".executeUpdate
      b <- "CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER, address VARCHAR(255))".executeUpdate
      p <- "INSERT INTO PERSON(name, age, address) VALUES (?,?,?)" prepareStatement
             (s => s.foreachStatement[List, Person](data, (p: Person) => p match {
               case Person(name, age, address) => {
                 s.set(stringType(name), intType(age), address toJDBCType stringType)
               }
             }))
    } yield a + b + p

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      // use file-based database
      def connection = com.ephox.vault.Connector.hsqlfile(args(0), args(1), args(2)).nu

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
      val combinedPerson = PersonSql.toRowAccess -||> combine <|- "SELECT * FROM PERSON".toSql finalyClose connection

      // print the result
      combinedPerson.println
    }
  }
}