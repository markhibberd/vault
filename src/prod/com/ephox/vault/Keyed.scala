package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait Keyed[A] {
  def get(a: A): Key
  def set(a: A, key: Key): A
}

trait Keyeds {
  def keyed[A](getf: A => Key, setf: (A, Key) => A): Keyed[A] = new Keyed[A] {
    def get(a: A) = getf(a)
    def set(a: A, key: Key) = setf(a, key)
  }
}

