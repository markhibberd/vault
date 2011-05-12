package com.ephox.vault

import org.scalatest.FunSuite
import scalaz._, Scalaz._
import com.ephox.vault._, Vault._

class SelectTest extends FunSuite {
  type L = String

  case class Fred(key: Key, name: String, value: Long) 

  val FredRowAccess = for {
    key <- keyLabel("ID")
    name <- stringLabel("NAME")
    value <- longLabel("VALUE")
  } yield Fred(key, name, value)

  val freds = List(
    new Fred(key(0), "f1", 1),
    new Fred(key(1), "f2", 2),
    new Fred(key(2), "f3", 3)
  )

  val populate = List(
    "CREATE TABLE FRED (ID IDENTITY, NAME VARCHAR(255), VALUE BIGINT)",
    "INSERT INTO FRED (NAME, VALUE) VALUES('f1', 1)",
    "INSERT INTO FRED (NAME, VALUE) VALUES('f2', 2)",
    "INSERT INTO FRED (NAME, VALUE) VALUES('f3', 3)"
  ).traverse(_.executeUpdate)

  test("raw insert, and check query") {
    val connection = Connectors.hsqltest.nu

    populate executeOrDie connection

    val result = Queries.list(FredRowAccess, "SELECT * FROM FRED".toSql) executeOrDie connection

    expect(freds)(result)
  }
}
