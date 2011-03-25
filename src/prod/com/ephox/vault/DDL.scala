package com.ephox.vault

import scalaz._
import Scalaz._

trait DDL {
  // todo trick to apply only one of the two parameters
  def executeUpdates[F[_], L](sqls: F[String])(implicit t: Traverse[F], fld: Foldable[F]): SqlConnect[L, Int] = {
    sqls.traverse[({type λ[α]= SqlConnect[L, α]})#λ, Int](_.executeUpdate) ∘ (_.sum)
  }
}