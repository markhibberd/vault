package com.ephox.vault

import scalaz._, Scalaz._

sealed trait Key {
  def fold[X](none: => X, some: Long => X): X

  def foldx[X](none: X, some: Long => X) = fold(none, some)

  def toOption = fold(None, Some(_))

  def valueOr(none: => Long) = fold(none, x => x)

  def valueOrx(none: Long) = valueOr(none)

  def isSet = fold(false, _ => true)

  def forall(p: Long => Boolean): Boolean =
    fold(true, p)

  def exists(p: Long => Boolean): Boolean =
    fold(false, p)

  override def toString = fold(
    "Key[]",
    "Key[" + _ + "]"
  )

  override def hashCode = fold(0, _.hashCode)

  override def equals(o: Any) =
    o.isInstanceOf[Key] && o.asInstanceOf[Key].fold(
      !isSet, value => exists(_ == value)
    )

  def idType = fold(JDBCType.nullType(NumericType), JDBCType.longType(_))
}

trait Keys {
  def key(value: Long): Key = new Key {
    def fold[X](none: => X, some: Long => X) = some(value)
  }

  def nokey: Key = new Key {
    def fold[X](none: => X, some: Long => X) = none
  }

  implicit def EqualKey: Equal[Key] = new Equal[Key] {
    def equal(k1: Key, k2: Key) =
      k1.fold(!k2.isSet, value => k2.fold(false, _ == value))
  }
  implicit def ShowKey: Show[Key] = new Show[Key] {
    def show(k: Key) = k.toString.toList
  }
}

/* Do not rename me to Key, breaks java'ry things at the moment */
object CompanionKey extends Keys


