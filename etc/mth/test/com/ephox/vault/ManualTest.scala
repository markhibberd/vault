package com.ephox.vault

import org.scalatest.FunSuite
import java.sql.ResultSet

class ManualTest extends FunSuite {
  import DdlStatement._
  import DmlStatement._
  import Queries._
  import Transaction._

  test("playing around") {
    val album = ddl("CREATE TABLE ALBUM (id IDENTITY, name VARCHAR(255), artist_id INTEGER)")
    val artist = ddl("CREATE TABLE ARTIST (id IDENTITY)")
    val fred = dml("INSERT INTO ALBUM(name, artist_id) VALUES (?, ?)", List("fred", 0))
    val barney = dml("INSERT INTO ALBUM(name, artist_id) VALUES (?, ?)", List("barney", 0))

    val tx = for {
      _ <- Transaction(executeDdl(_)(album))
      _ <- Transaction(executeDdl(_)(artist))
      id <- Transaction(executeDmlWithKeys(_)(fred, identifier))
      _ <- Transaction(executeDml(_)(barney))
      r <- Transaction(runQuery(_)(query("select * from album"), rs => mkAlbums(rs)))
    } yield r

    val s = evaluate(Connector.hsqltest, tx).fold(
      _.getMessage,
      _.map(row => "ok: " + row).mkString("\n")
    )

    println(s)
  }

  case class Album(id: Int, name: String, artistId: Int)

  def identifier(i: Int, rs: ResultSet) =
    if (rs.next) rs.getInt("id") else 0

  def mkAlbums (rs: ResultSet, acc: List[Album] = List()): List[Album] =
    if (rs.next) {
      mkAlbums(rs, acc ++ List(Album(rs.getInt("id"), rs.getString("name"), rs.getInt("artist_id"))))
    } else
      acc
}