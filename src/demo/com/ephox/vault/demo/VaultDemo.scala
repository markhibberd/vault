package com.ephox.vault.demo

import com.ephox.vault.Transaction
import com.ephox.vault.Transaction._
import com.ephox.vault.Connector.hsqltest

object VaultDemo {
  def mkschema = {
    val tx = for {
      ddl <- Transaction(executeDdl(_)(schema.demo))
    } yield ddl

    evaluate(hsqltest, tx).fold(
      bomb => bomb.getMessage,
      _ => "created schema succesfully..."
    )
  }


  def main(args: Array[String]) {
    println(mkschema)
  }
}
