package com.ephox.vault

import scalaz._, Scalaz._

/** A merger accepts two values of the same structure and either successfully merges them to a value of same structure or fails. */
trait Merger[A] {
  /**
   * Merge the two values.
   */
  val merge: (A, A) => Option[A]

  /** Merge the two values and return whether or not the merge succeeds. */
  def succeeds: (A, A) => Boolean =
    (a1, a2) => merge(a1, a2).isDefined

  /** Merge the two values and return whether or not the merge fails. */
  def fails: (A, A) => Boolean =
    (a1, a2) => !succeeds(a1, a2)

  /**
   * If the merge fails use the given default.
   */
  def mergeOr(a: => A): (A, A) => A =
    (a1, a2) => merge(a1, a2) getOrElse a

  /**
   * If the merge fails use the first value for default.
   */
  def mergeOrFirst: (A, A) => A =
    (a1, a2) => mergeOr(a1)(a1, a2)

  /**
   * If the merge fails use the second value for default.
   */
  def mergeOrSecond: (A, A) => A =
    (a1, a2) => mergeOr(a2)(a1, a2)

  /**
   * Merge the two values. Synonym for `merge`.
   */
  def apply(a1: A, a2: A) = merge(a1, a2)
}

object Merger extends Mergers {
  def apply[A](m: (A, A) => Option[A]): Merger[A] =
    merger(m)
}


trait Mergers {
  /** A synonym for a merger that always succeeds with a value */
  type SuccMerger[A] =
    (A, A) => A

  /** Construct a merger from the given function which takes two values and may successfully merge (`Some`) or fail to merge (`None`). */
  def merger[A](merger: (A, A) => Option[A]): Merger[A] = new Merger[A] {
    val merge = merger
  }

  /** A merger that does not inspect its values */
  def constant[A](c: => Option[A]) = merger[A]((_, _) => c)

  /** A merger that always fails to merge */
  def mergeNone[A]: Merger[A] = constant(None)

  /** A merger that always successfully merges with the given value */
  def mergeSome[A](a: => A): Merger[A] = constant(Some(a))

  /** A merger that always successfully merges using the given function */
  def someMerger[A](f: SuccMerger[A]) = merger[A]((a1, a2) => Some(f(a1, a2)))

  /** A merger that successfully merges using the given function, only if the given predicate holds. */
  def ifelseMerger[A](p: (A, A) => Boolean, t: SuccMerger[A]) = merger[A]((a1, a2) => if(p(a1, a2))  Some(t(a1, a2)) else None)

  /** A merger that successfully merges using the given function only if the ids of the two values are equivalent */
  def idMerger[A](t: SuccMerger[A])(implicit k: Keyed[A]) = ifelseMerger[A]((a, b) => k.get(a) == k.get(b), t)

  /** A merger that successfully merges to the first value only if the ids of the two values are equivalent. */
  def merge0[A](implicit k: Keyed[A]) = idMerger[A]((a1, _) => a1)

  /** A merger that successfully merges using the given merger and lens if the ids of the two values are equivalent, otherwise, succeeds with the first value. */
  def merge1[A, B](get: A => B, set: (A, B) => A)(implicit ka: Keyed[A], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, mb mergeOrFirst (get(a1), get(a2))))

  /** A merger that successfully merges using the given mergers and lenses if the ids of the two values are equivalent, otherwise, succeeds with the first value. */
  def merge2[A, B, C](getB: A => B, getC: A => C, set: (A, B, C) => A)(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, mb mergeOrFirst (getB(a1), getB(a2)), mc mergeOrFirst (getC(a1), getC(a2))))

  def merge1n[A, B](get: A => List[B], set: (A, List[B]) => A)(implicit ka: Keyed[A], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(get(a1), get(a2))))

  def merge2n[A, B, C](getB: A => List[B], getC: A => List[C], set: (A, List[B], List[C]) => A)(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)), listMerge(getC(a1), getC(a2))))

  def merge3n[A, B, C, D](getB: A => List[B], getC: A => List[C], getD: A => List[D], set: (A, List[B], List[C], List[D]) => A)(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C], md: Merger[D]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)), listMerge(getC(a1), getC(a2)), listMerge(getD(a1), getD(a2))))

  def merge1n1[A, B, C](getB: A => List[B], getC: A => C, set: (A, List[B], C) => A)(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)), mc(getC(a1), getC(a2)).getOrElse(getC(a1))))

  def lmerge1[A, B](k: Lens[A, B])(implicit ka: Keyed[A], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => k.set(a1, mb mergeOrFirst (k get a1, k get a2)))

  /** A merger that successfully merges using the given mergers and lenses if the ids of the two values are equivalent, otherwise, succeeds with the first value. */
  def lmerge2[A, B, C](k0: Lens[A, B], k1: Lens[A, C])(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => k1.set(k0.set(a1, mb mergeOrFirst (k0 get a1, k0 get a2)), mc mergeOrFirst (k1 get a1, k1 get a2)))

  def lmerge1n[A, B](k: Lens[A, List[B]])(implicit ka: Keyed[A], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => k.set(a1, listMerge(k get a1, k get a2)))

  def lmerge2n[A, B, C](k0: Lens[A, List[B]], k1: Lens[A, List[C]])(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => k1.set(k0.set(a1, listMerge(k0 get a1, k0 get a2)), listMerge(k1 get a1, k1 get a2)))

  def lmerge3n[A, B, C, D](k0: Lens[A, List[B]], k1: Lens[A, List[C]], k2: Lens[A, List[D]])(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C], md: Merger[D]): Merger[A] =
    idMerger[A]((a1, a2) => k2.set(k1.set(k0.set(a1, listMerge(k0 get a1, k0 get a2)), listMerge(k1 get a1, k1 get a2)), listMerge(k2 get a1, k2 get a2)))

  def lmerge1n1[A, B, C](k0: Lens[A, List[B]], k1: Lens[A, C])(implicit ka: Keyed[A], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => k1.set(k0.set(a1, listMerge(k0 get a1, k0 get a2)), mc mergeOrFirst (k1 get a1, k1 get a2)))

  def listMerge[A](x: List[A], y: List[A])(implicit merge: Merger[A]): List[A] =
    y.foldRight[List[A]](x)((a, acc) => valueMerge(acc, a))

  def valueMerge[A](x: List[A], y: A)(implicit merge: Merger[A]): List[A] =
    (x.foldRight((some(y), Nil: List[A])) {
      case (v, (None, acc)) => (None, v :: acc)
      case (v, (Some(a), acc)) =>
        merge(v, a) match {
          case None => (Some(a), v :: acc)
          case Some(u) => (None, u :: acc)
        }
    }) match {
      case (value, acc) => value match {
        case Some(w) => w :: acc
        case None    => acc
      }
    }
}
