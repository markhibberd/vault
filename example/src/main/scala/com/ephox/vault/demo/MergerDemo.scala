package com.ephox.vault.demo

import scalaz._, Scalaz._
import com.ephox.vault._, Vault._

object MergerDemo {
  def main(args: Array[String]) {
    case class Person(id: Key, name: String, age: Int)

    implicit def PersonKeyed: Keyed[Person] =
      keyed[Person](_.id, (x, k) => x.copy(id = k))

    implicit def PersonMerger: Merger[Person] =
    	merge0

    case class People(p: List[Person])

    implicit def PeopleMerger: Merger[People] =
      someMerger((p1, p2) => People(listMerge(p1.p, p2.p)))

    val pa =
      People(List(
        Person(key(7), "Bob", 14)
      , Person(nokey, "Mary", 17)
      , Person(key(2), "Bill", 23)
      ))

    val qa =
      People(List(
        Person(nokey, "Fred", 14)
      , Person(key(7), "Bob", 17)
      , Person(key(8), "Bob", 18)
      , Person(key(12), "Mary", 23)
      , Person(key(2), "Tom", 55)
      ))

    val ra =
      People(List(
        Person(key(8), "Bob", 18)
      , Person(key(12), "Mary", 23)
      , Person(key(7), "Bob", 14)
      , Person(nokey, "Mary", 17)
      , Person(key(2), "Bill", 23)
      ))

    assert(implicitly[Merger[People]] merge (pa, qa), Some(ra))
  }

  def assert[A](a1: A, a2: A) {
    if(a1 != a2)
      println(a1 + " ≠ " + a2)
  }
}
