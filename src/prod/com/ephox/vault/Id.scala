package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait Id {
  def fold[X](none: => X, some: Long => X): X

  def foldx[X](none: X, some: Long => X) = fold(none, some)

  def toOption = fold(None, Some(_))

  def valueOr(none: => Long) = fold(none, x => x)

  def valueOrx(none: Long) = valueOr(none)

  def isSet = fold(false, _ => true)
}

object Id {
  def id(value: Long): Id = new Id {
    def fold[X](none: => X, some: Long => X) = some(value)
  }

  def noid = new Id {
    def fold[X](none: => X, some: Long => X) = none
  }

  implicit def EqualId: Equal[Id] = equal((a, b) => a.toOption == b.toOption)
}
