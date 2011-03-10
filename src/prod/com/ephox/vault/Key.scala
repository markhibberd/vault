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

  override def toString = fold(
    "Key[]",
    "Key[" + _ + "]"
  )
}

trait Keys {
  def key(value: Long): Key = new Key {
    def fold[X](none: => X, some: Long => X) = some(value)
  }

  def nokey = new Key {
    def fold[X](none: => X, some: Long => X) = none
  }

  implicit def EqualKey: Equal[Key] =
    equal((a, b) => a.fold(!b.isSet, value => b.fold(false, _ == value)))

  implicit def ShowKey: Show[Key] =
    shows(_.toString)
}

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

trait KeyedW[A] {
  val value: A
  val keyed: Keyed[A]

  def id = keyed.get(value)

  def =@=(other: KeyedW[A])(implicit eq: Equal[Key]) =
    eq.equal(id, other.id)
}

trait KeyedWs {
  implicit def KeyedWTo[A](a: A)(implicit keyedf: Keyed[A]): KeyedW[A] = new KeyedW[A] {
    val value = a
    val keyed = keyedf
  }

  implicit def KeyedWFrom[A](a: KeyedW[A]): A = a.value
}
