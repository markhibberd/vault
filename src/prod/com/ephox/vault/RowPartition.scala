package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait RowPartition[L] {
  val partition: Row => Row => RowValue[L, Boolean]
}

object RowPartition {
  def rowPartition[L](f: Row => Row => RowValue[L, Boolean]): RowPartition[L] = new RowPartition[L] {
    val partition = f
  }

  def equalityPartition[L, A: Equal](k: Row => A): RowPartition[L] =
    rowPartition(r1 => r2 => (k(r1) === k(r2)).η[({type λ[α]= RowValue[L, α]})#λ])
}