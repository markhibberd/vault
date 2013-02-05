package vault

import java.sql.{PreparedStatement, Connection}

object Example {
  def run {
    val c = Connector.hsqltest.create()
    fake(c)
    val x = Execute.get[Unit, (String, Int, String)](c,
      "SELECT name, age, address FROM PERSON"
    , ())


    val r = x.fold(
      fail => "bomb: " + fail,
      ok => ok.toString
    )

    println(r)
  }


  // things in raw jdbc that are not implemented yet
  def fake(c: Connection) {
    def run(sql: String, set: PreparedStatement => Unit = _ => ()) {
      val s = c.prepareStatement(sql)
      set(s)
      s.executeUpdate()
    }
    run("DROP TABLE IF EXISTS PERSON")
    run("CREATE TABLE PERSON (id IDENTITY, name VARCHAR(255), age INTEGER, address VARCHAR(255))")
    run("INSERT INTO PERSON(name, age, address) VALUES (?,?,?)", stmt => {
      stmt.setString(1, "bob")
      stmt.setInt(2, 42)
      stmt.setString(3, "71 Somewhere Lane")
    })

  }

  def main(args: Array[String]) = run
}
