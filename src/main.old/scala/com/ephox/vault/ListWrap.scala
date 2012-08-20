package com.ephox.vault

import SqlConnect._
import RowConnect._
import scalaz._, Scalaz._

trait ListWrap {
  // FIX can we kill these with Free now?
  implicit def ListToConnectTraversable[A](l: List[A]) = new {
    def straverse[B](f: A => SqlConnect[B]) =
      foldTraverseSqlConnect(l, f)
    def rtraverse[B](f: A => RowConnect[B]) =
      foldTraverseRowConnect(l, f)
  }
}
