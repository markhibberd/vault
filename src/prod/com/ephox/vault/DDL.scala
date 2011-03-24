package com.ephox.vault

import scalaz._
import Scalaz._

trait DDL {
  def executeUpdates[F[_]](sqls: F[String])(implicit t: Traverse[F], fld: Foldable[F]): SqlConnect[Int] = {
    sqls.traverse(_.executeUpdate) ∘ (_.sum)
  }
}