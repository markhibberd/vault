package vault

import java.sql.{PreparedStatement, Connection}

object Example {
  def run {
    val c = Connector.hsqltest.create()
    fake(c)
    val x = Execute.list[Unit, (String, Int, String)]("SELECT name, age, address FROM PERSON", ())

    val r = x(c).fold(
      fail => "bomb: " + fail,
      { case (log, ok) => ok.toString }
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
    (1 to 10).foreach(i =>
      run("INSERT INTO PERSON(name, age, address) VALUES (?,?,?)", stmt => {
        stmt.setString(1, "bob" + i)
        stmt.setInt(2, 42)
        stmt.setString(3, "71 BigHouse Street")
      })

    )
  }

  def main(args: Array[String]) = run
}
