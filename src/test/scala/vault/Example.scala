package vault

import scalaz._, Scalaz._, effect.IO

import vault.Syntax._

object Example {
  case class Person(name: String, age: Int, address: String)

  /* could be elided by importing vault.FromDb.auto._ */
  implicit def PersonFromDb: FromDb[Person] =
    FromDb.derive[Person]

  /* could be elided by importing vault.ToDb.auto._ */
  implicit def PersonToDb: ToDb[Person] =
    ToDb.derive[Person]

  /* A 'Db' represents a series of computations against a data base.
     they are generally run together as a single transaction */
  def run: Db[Unit] = for {
    /*
     * Just some setup for the example, but demonstrates executing arbitrary sql.
     */
    _ <- q"DROP TABLE IF EXISTS person".execute
    _ <- q"DROP TABLE IF EXISTS migrations".execute

    /*
     * The dummest (and most incomplete) migrations library ever.
     */
    _ <- Migrations.hsqldb(Migrations(List(
      "initial-person-table" -> "CREATE TABLE person (id IDENTITY, name VARCHAR(255), age INTEGER, address VARCHAR(255))"
    )))

    /*
     * Insert some data, notice the argument gets unfolded into the params.
     */
    _ <- (1 to 300).toList.traverse(i =>
      q"INSERT INTO person(name, age, address) VALUES (${"bob" + i},${42},${"71 Big House Street"})".update)

    /*
     * Get multiple results from the db.
     */
    x <- q"SELECT name, age, address FROM person".list[(String, Int, String)]
    _ <- x.traverse(xx => Db.liftIO {
      IO.putStrLn(xx.shows)
    })

    /*
     * Get a single result from the db - also demonstrating bind parameters.
     */
    y <- q"SELECT name, age, address FROM person WHERE name = ${"bob3"}".get[(String, Int, String)]
    _ <- Db.liftIO { IO.putStrLn(y.shows) }

    /*
     * Also works with data types with FromDb.
     */
    z <- q"SELECT name, age, address FROM person WHERE name = ${"bob3"}".get[Person]
    _ <- Db.liftIO { IO.putStrLn(z.toString) }

    /*
     * And for inserting with ToDb.
     */
    p =  Person("fred", 123, "street")
    _ <- q"INSERT INTO person(name, age, address) VALUES (${p})".update
    z <- q"SELECT name, age, address FROM person WHERE name = ${"fred"}".get[Person]
    _ <- Db.liftIO { IO.putStrLn(z.toString) }

    /*
     * Or more explicitly with bind.
     */
    p =  Person("fred", 123, "street")
    _ <- q"INSERT INTO person(name, age, address) VALUES (?, ?, ?)".bind(p).update
    z <- q"SELECT name, age, address FROM person WHERE name = ${"fred"}".get[Person]
    _ <- Db.liftIO { IO.putStrLn(z.toString) }

  } yield ()

  def main(args: Array[String]) = {
    /*
     * `testLogDb` runs the sql in a transaction that always rolls back at the end.
     */
    val (log, v) = run.testLogDb(Connector.hsqltest).unsafePerformIO
    v.toEither match {
      case -\/(e) => println("oops: " + e); log.history.foreach(println)
      case \/-(v) => ()
    }
  }
}
