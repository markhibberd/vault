package com.ephox.vault.demo

import com.ephox.vault._

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