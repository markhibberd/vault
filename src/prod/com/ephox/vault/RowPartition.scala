package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowPartition {
  val partition: Row => Row => RowValue[Boolean]
}

object RowPartition {
  def rowPartition(f: Row => Row => RowValue[Boolean]): RowPartition = new RowPartition {
    val partition = f
  }

  def equalityPartition[A: Equal](k: Row => A): RowPartition =
    rowPartition(r1 => r2 => (k(r1) === k(r2)).η[RowValue])
}