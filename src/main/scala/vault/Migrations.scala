package vault

import scalaz.stream._
import java.sql.{Connection, SQLException}
import scalaz._, Scalaz._, effect._, Effect._, concurrent._
import DbValue.db
import Db._

case class Migrations(migrations: List[(String, String)])
case class MigrationEngine(initialize: Db[Unit], list: Db[List[String]], register: String => Db[Unit])

object Migrations {
  def run(engine: MigrationEngine)(migrations: Migrations): Db[List[String]] = for {
    _ <- engine.initialize
    e <- engine.list
    i <- pick(migrations.migrations, e).traverse({
      case (name, sql) => (Execute.execute_(sql) >> engine.register(name)).as(name)
    })
  } yield i

  def pick(available: List[(String, String)], installed: List[String]): List[(String, String)] = {
    val s = installed.toSet
    available.filter({ case (name, sql) => !s.contains(name) })
  }

  def postgres: Migrations => Db[List[String]] =
    run(MigrationEngine(
      Execute.execute_("CREATE TABLE IF NOT EXISTS migrations (migration CHARACTER VARYING PRIMARY KEY)").void
    , Execute.list_("SELECT migration FROM migrations")
    , name => Execute.execute("INSERT INTO migrations (migration) VALUES (?)", name).void
    ))

  def hsqldb: Migrations => Db[List[String]] =
    run(MigrationEngine(
      Execute.execute_("CREATE TABLE IF NOT EXISTS migrations (migration VARCHAR(255) PRIMARY KEY)").void
    , Execute.list_("SELECT migration FROM migrations")
    , name => Execute.execute("INSERT INTO migrations (migration) VALUES (?)", name).void
    ))
}
