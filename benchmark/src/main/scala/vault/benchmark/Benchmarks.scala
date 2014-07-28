package vault.benchmark

import com.google.caliper._

import java.sql._

import scalaz._, Scalaz._

import vault._

object ExecuteBenchApp extends App {
  Runner.main(classOf[ExecuteBench], args)
}

case class ExecuteBench() extends SimpleScalaBenchmark with DbBenchmark {
  def time_jdbc(n: Int) = db(n) { c =>
    val s = c.prepareStatement(Profile.insert)
    s.setString(1, "name")
    s.setInt(2, 1)
    s.executeUpdate
    val q = c.prepareStatement(Profile.delete)
    q.execute
  }

  def time_vault(n: Int) = db (n) { c =>
    (Execute.update(Profile.insert, ("name", 1)) >>
     Execute.execute_(Profile.delete)).run(DbRead.connect(c)).run.run
  }
}

object QueryBenchApp extends App {
  Runner.main(classOf[QueryBench], args)
}

case class QueryBench() extends SimpleScalaBenchmark with DbBenchmark {
  def time_jdbc(n: Int) = dbdata(n) { c =>
    val rs = c.prepareStatement("SELECT age FROM person").executeQuery
    var total = 0
    while (rs.next) { total += rs.getInt(1) }
    if (total != 55)
      sys.error("Bad " + total)
  }

  def time_vault(n: Int) = dbdata (n) { c =>
    val total = Execute.process[Unit, Int]("SELECT age FROM person", ()).runFoldMap(identity).run(DbRead.connect(c).withChunkSize(100)).run.run
    if (total != DbValue.ok(55))
      sys.error("Bad " + total)
  }

  def time_vault_no_buffer(n: Int) = dbdata (n) { c =>
    val total = Execute.process[Unit, Int]("SELECT age FROM person", ()).runFoldMap(identity).run(DbRead.connect(c).withChunkSize(1)).run.run
    if (total != DbValue.ok(55))
      sys.error("Bad " + total)
  }

}

trait DbBenchmark { self: SimpleScalaBenchmark =>

  def db[A](n: Int)(f: Connection => A): Int ={
    val c = Connector.hsqltest.create()
    Profile.setup(c)
    repeat(n) {
      f(c)
    }
    Profile.shutdown(c)
    n
  }

  def dbdata[A](n: Int)(f: Connection => A): Int ={
    val c = Connector.hsqltest.create()
    Profile.setup(c)
    Profile.data(c)
    repeat(n) {
      f(c)
    }
    Profile.shutdown(c)
    n
  }
}


object Profile {
  def table = "CREATE TABLE person (id IDENTITY, name VARCHAR(255), age INTEGER)"
  def insert = "INSERT INTO person (name, age) VALUES (?, ?)"
  def delete = "DELETE FROM person"

  def setup(c: Connection) =
    c.prepareStatement(table).execute

  def data(c: Connection, size: Int = 10) = {
    val s = c.prepareStatement(insert)
    (1 to size).foreach(n => {
      s.setString(1, "name" + n)
      s.setInt(2, n)
      s.executeUpdate
    })
  }

  def shutdown(c: Connection) = {
    c.prepareStatement("SHUTDOWN").execute
    c.close
  }

  def main(args: scala.Array[String]): Unit = {
    val c = Connector.hsqltest.create()
    setup(c)
    data(c, 1000)

    while (true)
      Execute.process[Unit, Int]("SELECT age FROM person", ()).runFoldMap(identity).run(DbRead.connect(c).withChunkSize(1000)).run.run

    shutdown(c)
  }
}
