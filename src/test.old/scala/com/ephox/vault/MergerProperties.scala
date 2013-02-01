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

  implicit def PersonMerger: Merger[Person] =
    merge0

  case class CarMake(id: Key, name: String)

  implicit def CarMakeKeyed: Keyed[CarMake] =
    keyed[CarMake](_.id, (x, k) => x.copy(id = k))

  implicit def CarMakeMerger: Merger[CarMake] =
    merge0

  implicit val ArbitraryCarMake: Arbitrary[CarMake] =
    Arbitrary(implicitly[Arbitrary[(Key, String)]].arbitrary map { case (k, m) => CarMake(k, m) })

  case class Colour(id: Key, name: String)

  implicit def ColourKeyed: Keyed[Colour] =
    keyed[Colour](_.id, (x, k) => x.copy(id = k))

  implicit def ColourMerger: Merger[Colour] =
    merge0

  implicit val ArbitraryColour: Arbitrary[Colour] =
    Arbitrary(implicitly[Arbitrary[(Key, String)]].arbitrary map { case (k, n) => Colour(k, n) })


  case class Car(id: Key, make: CarMake, driver: Person, passengers: List[Person], colours: List[Colour])

  implicit val ArbitraryCar: Arbitrary[Car] =
    Arbitrary(implicitly[Arbitrary[(Key, CarMake, Person, List[Person], List[Colour])]].arbitrary map { case (k, m, d, p, c) => Car(k, m, d, p, c) })

  implicit def CarKeyed: Keyed[Car] =
    keyed[Car](_.id, (x, k) => x.copy(id = k))

  implicit def CarMerger: Merger[Car] =
    merge0

  // Turn it up
  override def check(p: Test.Params) {
    super.check(p.copy(
      minSuccessfulTests = 10000
    , maxDiscardedTests = 10000
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

  property("merge1n has same ids for successful merge") =
    forAll((x: Car
          , y: Car) =>
      merge1n[Car, Person](_.passengers, (c, p) => c.copy(passengers = p)) merge (x, y)
        forall (c => {
          val m = implicitly[Merger[Person]] mergeOrFirst (x.driver, y.driver)
          // merged drivers have the same id as the result and the first
          val p = m.id == c.driver.id && m.id == x.driver.id
          // the result has the same id as the merged cars
          val q = (c.id == x.id) && (c.id == y.id)
          p && q
        }))

  property("merge2n has same ids for successful merge") =
    forAll((x: Car
          , y: Car) =>
      merge2n[Car, Person, Colour](_.passengers, _.colours, (c, p, r) => c.copy(passengers = p, colours = r)) merge (x, y)
        forall (c => {
          val m = implicitly[Merger[Person]] mergeOrFirst (x.driver, y.driver)
          // merged drivers have the same id as the result and the first
          val p = m.id == c.driver.id && m.id == x.driver.id
          // the result has the same id as the merged cars
          val q = (c.id == x.id) && (c.id == y.id)
          p && q
        }))

  property("merge1n1 has same ids for successful merge") =
    forAll((x: Car
          , y: Car) =>
      merge1n1[Car, Person, Person](_.passengers, _.driver, (c, r, p) => c.copy(passengers = r, driver = p)) merge (x, y)
        forall(c => {
          val m = implicitly[Merger[Person]] mergeOrFirst (x.driver, y.driver)
          // merged drivers have the same id as the result and the first
          val p = m.id == c.driver.id && m.id == x.driver.id
          // the result has the same id as the merged cars
          val q = (c.id == x.id) && (c.id == y.id)
          p && q
        }))


  property("listMerge merges two lists") =
    forAll((x: List[Car]
          , y: List[Car]) => {
      val r = listMerge(x, y)
      val z = {
        val xs = x.toSet // optimisation only

        // CoState[A, Key]
        // Useful in vault library?
        case class KeyBy[A](value: A, by: A => Key) {
          override def equals(o: Any) =
            o.isInstanceOf[KeyBy[_]] && {
              val b = o.asInstanceOf[KeyBy[_]]
                b.by.asInstanceOf[A => Key](b.value.asInstanceOf[A]) == by(value)
            }

          override def hashCode =
            by(value).hashCode

          override def toString =
            by(value).toString
        }

        // Remove all elements in y that have the same key as any element in x
        val z0 = y.filterNot(c => xs.map(_.id).contains(c.id))

        // Remove any elements in z0 that have duplicate ids, keeping the last occurrence
        val z1 = z0.reverse.map(KeyBy[Car](_, _.id)).distinct.map(_.value).reverse

        // Append x to z1
        val z2 = z1 ::: x

        z2
      }

      z == r
    })

  property("valueMerge merges a key") =
    forAll((x: List[Car]
          , y: Car) => {
      val r = valueMerge(x, y)
      val xs = x.toSet // optimisation only
      ((x == r) == (xs.map(_.id) contains y.id)) && (r.map(_.id) contains y.id)
    })
}
