package com.ephox.vault

import scalaz._, Scalaz._

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

  override def hashCode = fold(0, _.hashCode)

  override def equals(o: Any) =
    o.isInstanceOf[Key] && o.asInstanceOf[Key].fold(
      !isSet, value => fold(false, _ == value)
    )
}

trait Keys {
  val x = 7
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

object Key extends Keys


