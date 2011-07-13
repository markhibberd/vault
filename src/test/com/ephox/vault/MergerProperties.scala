package com.ephox.vault

import org.scalacheck._, Prop._

object MergerProperties extends Properties("Merger") {
  implicit def ArbitraryMerger[A: Arbitrary]: Arbitrary[Merger[A]] =
    Arbitrary(implicitly[Arbitrary[(A, A) => Option[A]]].arbitrary map (Merger(_)))

  import Merger._

  property("succeeds is not fails") =
    forAll((m: Merger[Int], x: Int, y: Int) =>
      (m succeeds (x, y)) != (m.fails (x, y)))

  property("apply synonymous with merge") =
    forAll((m: Merger[Int], x: Int, y: Int) =>
      (m apply (x, y)) == (m merge (x, y)))

  property("mergeOr succeeds or uses default") =
    forAll((m: Merger[Int], d: Int, x: Int, y: Int) =>
      (m succeeds (x, y)) || (m.mergeOr(d)(x, y) == d))

  property("mergeOrFirst succeeds or uses first") =
    forAll((m: Merger[Int], d: Int, x: Int, y: Int) =>
      (m succeeds (x, y)) || (m.mergeOrFirst(x, y) == x))

  property("mergeOrSecond succeeds or uses second") =
    forAll((m: Merger[Int], d: Int, x: Int, y: Int) =>
      (m succeeds (x, y)) || (m.mergeOrSecond(x, y) == y))

  property("merger is the general constructor") =
    forAll((m: (Int, Int) => Option[Int], x: Int, y: Int) =>
      merger(m).merge(x, y) == m(x, y))

  property("someMerger always succeeds") =
    forAll((m: SuccMerger[Int], x: Int, y: Int) =>
      someMerger(m) succeeds (x, y))
}
