package com.ephox.vault.demo

import com.ephox.vault._

//case class Album(id: Int, name: String, songs: List[Song], artist: Band)
//case class Song(id: Int, name: String, albumId: Int)
//case class Band(id: Int, name: String)
//case class BandMuso(id: Int, bandId: Int, musoId: Int)
//case class Muso(id: Int, name: String, instrument: String)

object schema {
  import DdlStatement._

  val demo = ddls(List(
    "create table album (id IDENTITY, name VARCHAR(255), band_id INTEGER)",
    "create table song (id IDENTITY, name VARCHAR(255), album_id INTEGER)",
    "create table band (id IDENTITY, name VARCHAR(255))",
    "create table muso (id IDENTITY, name VARCHAR(255), instrument VARCHAR(255))",
    "create table band_muso (id IDENTITY, band_id INTEGER, muso_id INTEGER)"
  ))

}