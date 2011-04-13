package com.ephox.vault.demo

import scalaz._, Scalaz._
import com.ephox.vault._, Vault._

object VaultNullDemo {
  case class Person(name: String, age: Int)

  object Person {
    implicit val ShowPerson: Show[Person] = showA
  }

  val PersonRowAccess =
    for {
      name    <- stringIndex(2)
      age     <- intIndex(3)
    } yield Person(name, age)

  def setupData =
    for {
      b <- "CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER, address VARCHAR(255))".executeUpdate
      p <- "INSERT INTO PERSON(name, age) VALUES (?,?)" executePreparedUpdate (
//          _.set(stringType("name"), intType(30))
          _.set(stringType(null), intType(30))
//          _.set(stringType("name"), nullType(IntegerType))
        )
    } yield b + p

  def main(args: Array[String]) {
    def connection = com.ephox.vault.Connectors.hsqltest.nu

    // initialise data
    setupData commitRollback connection printStackTraceOr (n => println(n + " rows affected"))

    // get result and close connection
    val nuller = PersonRowAccess -||> IterV.head[Person] <|- "SELECT * FROM PERSON".toSql finalyClose

    val x = nuller connect connection

    val result = x.foldOrNullMsg("Null <no message>")(
      err => "Error: " + err,
      value => "Value: " + value,
      nully => "Null:" + nully
    )

    println(result)
  }
}