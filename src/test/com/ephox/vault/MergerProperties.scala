package com.ephox.vault

import org.scalacheck._, Prop._
import Vault._

object MergerProperties extends Properties("Merger") {
  implicit def ArbitraryMerger[A: Arbitrary]: Arbitrary[Merger[A]] =
    Arbitrary(implicitly[Arbitrary[(A, A) => Option[A]]].arbitrary map (Merger(_)))

  implicit def ArbitraryKey: Arbitrary[Key] =
    Arbitrary(implicitly[Arbitrary[Option[Long]]].arbitrary map {
      case None => nokey
      case Some(v) => key(v)
    })

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
    forAll((m: SuccMerger[Key]
          , x: Key
          , y: Key) =>
      (idMerger(m) succeeds (x, y)) == (x == y))

  property("merge0 succeeds with first if ids equal") =
    forAll((x: Key
          , y: Key) =>
      merge0[Key].merge(x, y) == (if(x == y) Some(x) else None))

}
