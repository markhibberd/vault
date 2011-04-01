package com.ephox.vault.demo

import scalaz._, Scalaz._
import com.ephox.vault._, Vault._

object VaultJoinDemo {
  // one-to-many Album -> Song
  // many-to-many Band -> Muso (via BandMuso)
  // many-to-one Album -> Band
  case class Song(id: Int, name: String, albumId: Int)
  case class Album(id: Int, name: String, songs: List[Song], band: Band)
  case class Band(id: Int, name: String)
  case class BandMuso(id: Int, bandId: Int, musoId: Int)
  case class Muso(id: Int, name: String, instrument: String)

  object Data {
    val musos = List(
      Muso(0, "muso0", "instr0")
    , Muso(1, "muso1", "instr1")
    , Muso(2, "muso2", "instr2")
    , Muso(3, "muso3", "instr3")
    )

    val albums = List(
      Album(0, "album0", Nil, Band(0, "band0"))
    , Album(1, "album1", List(
                               Song(0, "song0", 1)
                             ), Band(1, "band1"))
    , Album(2, "album2", List(
                               Song(2, "song2", 2)
                             , Song(3, "song3", 2)
                             , Song(4, "song4", 2)
                             ), Band(2, "band1"))
    )

    val bandMusos = List(
      BandMuso(0, 0, 0)
    , BandMuso(1, 1, 0)
    , BandMuso(2, 0, 1)
    , BandMuso(3, 2, 2)
    )

    val bands =
      albums map (_.band)

    val songs =
      albums flatMap (_.songs)
  }


  def setupData: SqlConnect[Int] = {
    val creates = List(
      "create table album (id IDENTITY, name VARCHAR(255), band_id INTEGER)"
    , "create table song (id IDENTITY, name VARCHAR(255), album_id INTEGER)"
    , "create table band (id IDENTITY, name VARCHAR(255))"
    , "create table muso (id IDENTITY, name VARCHAR(255), instrument VARCHAR(255))"
    , "create table band_muso (id IDENTITY, band_id INTEGER, muso_id INTEGER)"
    )

    for {
      n <- executeUpdates(creates)
      o <- "insert into muso(id, name, instrument) values (?,?,?)" prepareStatement (s => s.foreachStatement[List, Muso](Data.musos, (m: Muso) => m match {
             case Muso(id, name, instrument) => {
               s.set(intType(id), stringType(name), stringType(instrument))
             }
           }))

    } yield n + o
  }

  def main(args: Array[String]) {
    if(args.length < 3)
      System.err.println("<dbfile> <username> <password>")
    else {
      // use file-based database
      def connection = com.ephox.vault.Connectors.hsqlfile(args(0), args(1), args(2)).nu

      // initialise data
      setupData commitRollbackClose connection printStackTraceOr (n => println(n + " rows affected"))
    }
  }
}
