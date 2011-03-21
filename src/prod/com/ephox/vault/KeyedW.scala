package com.ephox.vault

import scalaz._
import Scalaz._

trait KeyedW[A] {
  val value: A
  val keyed: Keyed[A]

  def id = keyed.get(value)

  def =@=(other: KeyedW[A])(implicit eq: Equal[Key]) =
    eq.equal(id, other.id)

  def eqid(other: KeyedW[A])(implicit eq: Equal[Key]) =
    this =@= other
}

trait KeyedWX {
  implicit def KeyedWTo[A](a: A)(implicit keyedf: Keyed[A]): KeyedW[A] = new KeyedW[A] {
    val value = a
    val keyed = keyedf
  }

  implicit def KeyedWFrom[A](a: KeyedW[A]): A = a.value
}

