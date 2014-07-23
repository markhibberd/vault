package vault

import scalaz._, Scalaz._, effect.IO

object Example {
  /* A 'Db' represents a series of computations against a data base.
     they are generally run together as a single transaction */
  def run: Db[Unit] = for {
    /*
     * Just some setup for the example, but demonstrates executing arbitrary sql.
     */
    _ <- Execute.execute_("DROP TABLE IF EXISTS person")
    _ <- Execute.execute_("DROP TABLE IF EXISTS migrations")

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
      Execute.execute("INSERT INTO person(name, age, address) VALUES (?,?,?)",
        ("bob" + i, 42, "71 BigHouse Street")))

    /*
     * Get multiple results from the db.
     */
    x <- Execute.list_[(String, Int, String)]("SELECT name, age, address FROM person")
    _ <- x.traverse(xx => Db.liftIO {
      IO.putStrLn(xx.shows)
    })

    /*
     * Get a single result from the db - also demonstrating bind parameters.
     */
    y <- Execute.get[String, (String, Int, String)]("SELECT name, age, address FROM person WHERE name = ?", "bob3")
    _ <- Db.liftIO { IO.putStrLn(y.shows) }

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
