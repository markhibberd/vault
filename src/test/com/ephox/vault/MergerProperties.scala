package com.ephox.vault

import org.scalacheck._, Prop._
import Vault._

object MergerProperties extends Properties("Merger") {
  implicit def ArbitraryMerger[A: Arbitrary]: Arbitrary[Merger[A]] =
    Arbitrary(implicitly[Arbitrary[(A, A) => Option[A]]].arbitrary map (Merger(_)))

  implicit val ArbitraryKey: Arbitrary[Key] =
    Arbitrary(implicitly[Arbitrary[Option[Long]]].arbitrary map {
      case None => nokey
      case Some(v) => key(v)
    })

  case class Person(id: Key, name: String, age: Int)

  implicit val ArbitraryPerson: Arbitrary[Person] =
    Arbitrary(implicitly[Arbitrary[(Key, String, Int)]].arbitrary map { case (k, n, a) => Person(k, n, a) })

  implicit def PersonKeyed: Keyed[Person] =
    keyed[Person](_.id, (x, k) => x.copy(id = k))

  case class CarMake(id: Key, s: String)

  implicit def CarMakeKeyed: Keyed[CarMake] =
    keyed[CarMake](_.id, (x, k) => x.copy(id = k))

  implicit val ArbitraryCarMake: Arbitrary[CarMake] =
    Arbitrary(implicitly[Arbitrary[(Key, String)]].arbitrary map { case (k, m) => CarMake(k, m) })

  case class Car(id: Key, make: CarMake, driver: Person)

  implicit val ArbitraryCar: Arbitrary[Car] =
    Arbitrary(implicitly[Arbitrary[(Key, CarMake, Person)]].arbitrary map { case (k, m, d) => Car(k, m, d) })

  implicit def CarKeyed: Keyed[Car] =
    keyed[Car](_.id, (x, k) => x.copy(id = k))

  // Turn it up
  override def check(p: Test.Params) {
    super.check(p.copy(
      minSuccessfulTests = 1000
    , maxDiscardedTests = 1000
    ))
  }

  property("succeeds is not fails") =
    forAll((m: Merger[Int]
          , x: Int
          , y: Int) =>
      (m succeeds (x, y)) != (m.fails (x, y)))

  property("apply synonymous with merge") =
    forAll((m: Merger[Int]
          , x: Int
          , y: Int) =>
      (m apply (x, y)) == (m merge (x, y)))

  property("mergeOr succeeds or uses default") =
    forAll((m: Merger[Int]
          , d: Int
          , x: Int
          , y: Int) =>
      (m succeeds (x, y)) || (m.mergeOr(d)(x, y) == d))

  property("mergeOrFirst succeeds or uses first") =
    forAll((m: Merger[Int]
          , d: Int
          , x: Int
          , y: Int) =>
      (m succeeds (x, y)) || (m.mergeOrFirst(x, y) == x))

  property("mergeOrSecond succeeds or uses second") =
    forAll((m: Merger[Int]
          , d: Int
          , x: Int
          , y: Int) =>
      (m succeeds (x, y)) || (m.mergeOrSecond(x, y) == y))

  property("merger is the general constructor") =
    forAll((m: (Int, Int) => Option[Int]
          , x: Int
          , y: Int) =>
      merger(m).merge(x, y) == m(x, y))

  property("constant is the anonymous constructor") =
    forAll((m: Option[Int]
          , x: Int
          , y: Int) =>
      constant(m).merge(x, y) == m)

  property("mergeSome is the anonymous succeeding merger") =
    forAll((m: Int
          , x: Int
          , y: Int) =>
      mergeSome(m).merge(x, y) == Some(m))

  property("someMerger always succeeds") =
    forAll((m: SuccMerger[Int]
          , x: Int
          , y: Int) =>
      someMerger(m) succeeds (x, y))

  property("ifelseMerger succeeds using predicate") =
    forAll((p: (Int, Int) => Boolean
          , m: SuccMerger[Int]
          , x: Int
          , y: Int) =>
      (ifelseMerger(p, m) succeeds (x, y)) == p(x, y))

  property("idMerger succeeds if ids equal") =
    forAll((m: SuccMerger[Person]
          , x: Person
          , y: Person) =>
      (idMerger(m) succeeds (x, y)) == (x.id == y.id))

  property("merge0 succeeds with first if ids equal") =
    forAll((x: Person
          , y: Person) =>
      merge0[Person].merge(x, y) == (if(x.id == y.id) Some(x) else None))

  property("merge1 has same ids for successful merge") =
    forAll((x: Car
          , y: Car) =>
      merge1[Car, Person](_.driver, (c, p) => c.copy(driver = p)) merge (x, y)
        forall(c => {
          val m = implicitly[Merger[Person]] mergeOrFirst (x.driver, y.driver)
          // merged drivers have the same id as the result and the first
          val p = m.id == c.driver.id && m.id == x.driver.id
          // the result has the same id as the merged cars
          val q = (c.id == x.id) && (c.id == y.id)
          p && q
        }))

  property("merge2 has same ids for successful merge") =
    forAll((x: Car
          , y: Car) =>
      merge2[Car, Person, CarMake](_.driver, _.make, (c, d, m) => c.copy(driver = d, make = m)) merge (x, y)
        forall (c => {
          val m = implicitly[Merger[Person]] mergeOrSecond (x.driver, y.driver)
          // merged drivers have the same id as the second
          val p = m.id == y.driver.id
          // the result has the same id as the merged cars
          val q = (c.id == x.id) && (c.id == y.id)
          p && q
        }))

}
