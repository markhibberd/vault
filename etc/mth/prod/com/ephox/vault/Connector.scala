package com.ephox.vault

import javax.sql.DataSource
import java.sql._
import javax.naming.{Context, InitialContext}

sealed trait Connector {
  def nu: Connection
}

object Connector {
  def hsqltest: SqlConnect = hsqlmem("testdb", "sa", "")

  def hsqlmem(dbname: String, username: String, password: String):SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "org.hsqldb.jdbcDriver",
      "jdbc:hsqldb:mem:" + dbname,
      username, password
    )

    override def toString = "hsqlmem[" + username + "@" + dbname + "]"
  }

  def hsqlfile(dbfile: String, username: String, password: String):SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "org.hsqldb.jdbcDriver",
      "jdbc:hsqldb:file:" + dbfile,
      username, password
    )

    override def toString = "hsqlfile[" + username + "@" + dbfile + "]"
  }

  def webspherejndi(name: String) = {
    val env = new java.util.Hashtable[String, String]
    env.put(Context.INITIAL_CONTEXT_FACTORY, "com.ibm.ejs.ns.jndi.CNInitialContextFactory")
    val ctx = new InitialContext(env)
    jndi(ctx, name)
  }

  def jndi(ctx: InitialContext, name: String) = {
    val ds = ctx.lookup(name).asInstanceOf[DataSource]
    datasource(ds, name)
  }

  def datasource(ds: DataSource, name: String = "provided"): SqlConnect = new SqlConnect {
    def nu = ds.getConnection

    override def toString = "datasource[" + name + "]"
  }

  def oracle(hostname: String, port: Int, sid: String, username: String, password: String): SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "oracle.jdbc.OracleDriver",
      "jdbc:oracle:thin:@" + hostname + ":" + port + ":" + sid,
      username, password
    )

    override def toString = "oracle[" + username + "@" + hostname + ":" + port + ":" + sid + "]"
  }

  def db2(hostname: String, port: Int, dbname: String, username: String, password: String): SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "com.ibm.db2.jcc.DB2Driver",
      "jdbc:db2://" + hostname + ":" + port + "/" + dbname,
      username, password
    )

    override def toString = "db2[" + username + "@" + hostname + ":" + port + "/" + dbname + "]"
  }

  def mysql(hostname: String, port: Int, dbname: Int, username: String, password: String): SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "com.mysql.jdbc.Driver",
      "jdbc:mysql://" + hostname + ":" + port + "/" + dbname,
      username, password
    )

    override def toString = "mysql[" + username + "@" + hostname + ":" + port + "/" + dbname + "]"
  }

  def sqlserver(hostname: String, port: Int, dbname: Int, username: String, password: String):  SqlConnect = new SqlConnect {
    def nu = mkConnection(
      "com.microsoft.jdbc.sqlserver.SQLServerDriver",
      "jdbc:microsoft:sqlserver://" + hostname + ":" + port + ";databaseName=" + dbname,
      username, password
    )

    override def toString = "sqlserver[" + username + "@" + hostname + ":" + port + "/" + dbname + "]"
  }

  def postgres(hostname: String, port: Int, dbname: Int, username: String, password: String): SqlConnect = new SqlConnect {
    def nu = mkConnection(
    "org.postgresql.Driver",
    "jdbc:postgresql://" + hostname + ":" + port + "/" + dbname,
    username, password
    )

    override def toString = "postgres[" + username + "@" + hostname + ":" + port + "/" + dbname + "]"
  }

  def generic(driver: String, url: String, username: String, password: String): SqlConnect  = new SqlConnect {
    def nu = mkConnection(driver, url, username, password)

    override def toString = "generic[" + username + "@" + url + "]"
  }

  def mkConnection(driver: String, url: String, username: String, password: String): Connection = {
    Class.forName(driver)
    DriverManager.getConnection(url, username, password)
  }
}