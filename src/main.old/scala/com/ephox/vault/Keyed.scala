package com.ephox.vault

import scalaz._, Scalaz._, CostateT._

// FIX this needs to be updated, this really needs to be equiv of: data Keyed a = Keyed Key a
//     This is for a couple of reasons but mainly so we can use the types to demand things that
//     are already in database vs when we don't care
sealed trait Keyed[A] {
  def get(a: A): Key
  def set(a: A, key: Key): A
  def lens: Lens[A, Key] =
    Lens(t => costate(set(t, _), get(t)))
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
