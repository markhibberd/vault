package com.ephox.vault.demo

case class Song(id: Int, name: String, albumId: Int)
case class Album(id: Int, name: String, songs: List[Song], band: Band)
case class Band(id: Int, name: String)
case class BandMuso(id: Int, bandId: Int, musoId: Int)
case class Muso(id: Int, name: String, instrument: String)
