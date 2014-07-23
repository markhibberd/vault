package vault

import scalaz._, Scalaz._, effect.IO

object Example {
  def run: Db[Unit] = for {
    _ <- Execute.execute_("DROP TABLE IF EXISTS person")
    _ <- Execute.execute_("DROP TABLE IF EXISTS migrations")
    _ <- Migrations.hsqldb(Migrations(List(
      "initial-person-table" -> "CREATE TABLE person (id IDENTITY, name VARCHAR(255), age INTEGER, address VARCHAR(255))"
    )))
    _ <- (1 to 300).toList.traverse(i =>
      Execute.execute("INSERT INTO PERSON(name, age, address) VALUES (?,?,?)",
        ("bob" + i, 42, "71 BigHouse Street")))
    x <- Execute.list_[(String, Int, String)]("SELECT name, age, address FROM PERSON")
    _ <- x.traverse(xx => Db.liftIO {
      IO.putStrLn(xx.shows)
    })
  } yield ()

  def main(args: Array[String]) = {
    val (log, v) = run.testLogDb(Connector.hsqltest.create()).unsafePerformIO
    v.toEither match {
      case -\/(e) => println("oops: " + e); log.history.foreach(println)
      case \/-(v) => ()
    }
  }
}
