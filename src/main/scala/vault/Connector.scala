package vault

import java.sql._

case class Connector(create: () => Connection)

object Connector {
  def postgres(hostname: String, port: Int, dbname: String, username: String, password: String) =
    raw("org.postgresql.Driver", "jdbc:postgresql://" + hostname + ":" + port + "/" + dbname, username, password)

  def mysql(hostname: String, port: Int, dbname: String, username: String, password: String) =
    raw("com.mysql.jdbc.Driver", "jdbc:mysql://" + hostname + ":" + port + "/" + dbname, username, password)

  def hsqltest =
    hsqlmem("testdb", "sa", "")

  def hsqlmem(dbname: String, username: String, password: String) =
    raw("org.hsqldb.jdbcDriver", "jdbc:hsqldb:mem:" + dbname, username, password)

  def hsqlfile(dbfile: String, username: String, password: String) =
    raw("org.hsqldb.jdbcDriver", "jdbc:hsqldb:file:" + dbfile, username, password)

  def raw(driver: String, url: String, username: String, password: String): Connector = Connector(() => {
    Class.forName(driver)
    val c = DriverManager.getConnection(url, username, password)
    c.setAutoCommit(false)
    c
  })

}
