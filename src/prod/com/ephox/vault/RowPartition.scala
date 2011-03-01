package com.ephox.vault

import scalaz._
import Scalaz._
import Vault._

sealed trait RowPartition {
  val partition: Row => Row => RowAccess[Boolean]
}

object RowPartition {
  def rowPartition(f: Row => Row => RowAccess[Boolean]): RowPartition = new RowPartition {
    val partition = f
  }

  def equalityPartition[A: Equal](k: Row => A) =
    rowPartition(r1 => r2 => (k(r1) === k(r2)).η[RowAccess])
}