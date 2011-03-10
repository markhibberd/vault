package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait Key {
  def fold[X](none: => X, some: Long => X): X

  def foldx[X](none: X, some: Long => X) = fold(none, some)

  def toOption = fold(None, Some(_))

  def valueOr(none: => Long) = fold(none, x => x)

  def valueOrx(none: Long) = valueOr(none)

  def isSet = fold(false, _ => true)
}


object Key {
  def key(value: Long): Key = new Key {
    def fold[X](none: => X, some: Long => X) = some(value)
  }

  def nokey = new Key {
    def fold[X](none: => X, some: Long => X) = none
  }

  implicit def EqualKey: Equal[Key] =
    equal((a, b) => a.fold(b.isSet, value => b.fold(false, _ == value)))
}

sealed trait Keyed[A] {
  import Key._

  val value: A
  def get: Key
  def set(key: Key): A
  def =@=(keyed: Keyed[A])(implicit eq: Equal[Key]) = eq.equal(get, keyed.get)
}

object Keyed {
  def idd[A](a: A, getf: A => Key, setf: (A, Key) => A): Keyed[A] = new Keyed[A] {
    val value = a
    def get = getf(value)
    def set(key: Key) = setf(value, key)
  }
}
