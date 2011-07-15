package com.ephox.vault

import scalaz._, Scalaz._

// ≈ Lens[A, Key]
sealed trait Keyed[A] {
  def get(a: A): Key
  def set(a: A, key: Key): A
  def lens: Lens[A, Key] =
    Lens(get(_), set(_, _))
}

object Keyed extends Keyeds

trait Keyeds {
  def keyed[A](getf: A => Key, setf: (A, Key) => A): Keyed[A] = new Keyed[A] {
    def get(a: A) = getf(a)
    def set(a: A, key: Key) = setf(a, key)
  }

  def keyedLens[A](k: Lens[A, Key]): Keyed[A] =
    keyed(k.get, k.set)

  implicit def KeyKeyed: Keyed[Key] =
    keyed(z => z, (_, k) => k)
}
