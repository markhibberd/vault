package com.ephox.vault

import scalaz._, Scalaz._

trait Updates {
  import StringQuery._

  def executeUpdates[F[_]](sqls: F[String])(implicit t: Traverse[F], fld: Foldable[F]): SqlConnect[Int] = {
    sqls.traverse[SqlConnect, Int](_.executeUpdate) ∘ (_.sum)
  }
}

object Update extends Updates
