package com.ephox
package vault
package sql

import scalaz._, Scalaz._

sealed trait SqlT[F[+_], +A] {
  val run: JSqlT[F, A] \/ SSqlT[F, A]
}
