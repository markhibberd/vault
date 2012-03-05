package com.ephox.vault

import SqlConnect._
import RowConnect._

trait ListWrap {
  implicit def ListToConnectTraversable[A](l: List[A]) = new {
    def straverse[B](f: A => SqlConnect[B]) =
      foldTraverseSqlConnect(l, f)
    def rtraverse[B](f: A => RowConnect[B]) =
      foldTraverseRowConnect(l, f)
  }
}