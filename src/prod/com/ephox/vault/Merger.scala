package com.ephox.vault

import com.ephox.vault._, KeyedW._
import scalaz._, Scalaz._
import CampanionKey._

trait Merger[A] {
  val merge: (A, A) => Option[A]

  def apply(a1: A, a2: A) = merge(a1, a2)
}

object Merger extends Mergers

trait Mergers {
  def merger[A](merger: (A, A) => Option[A]): Merger[A] = new Merger[A] {
    val merge = merger
  }

  def constant[A](c: => Option[A]) = merger[A]((_, _) => c)

  def mergeNone[A]: Merger[A] = constant(None)

  def mergeSome[A](a: => A): Merger[A] = constant(Some(a))

  def someMerger[A](f: (A, A) => A) = merger[A]((a1, a2) => Some(f(a1, a2)))

  def ifelseMerger[A](p: (A, A) => Boolean, t: (A, A) => A) = merger[A]((a1, a2) => if(p(a1, a2))  Some(t(a1, a2)) else None)

  def idMerger[A](t: (A, A) => A)(implicit k: Keyed[A]) = ifelseMerger[A](_ =@= _, t)

  def merge0[A](implicit k: Keyed[A]) = idMerger[A]((a1, a2) => a1)

  def merge1[A, B](get: A => B, set: (A, B) => A)(implicit ka: Keyed[A], kb: Keyed[B], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, mb(get(a1), get(a2)).getOrElse(get(a1))))

  def merge2[A, B, C](getB: A => B, getC: A => C, set: (A, B, C) => A)(implicit ka: Keyed[A], kb: Keyed[B], kc: Keyed[C], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, mb(getB(a1), getB(a2)).getOrElse(getB(a1)), mc(getC(a1), getC(a2)).getOrElse(getC(a1))))

  def merge1n[A, B](get: A => List[B], set: (A, List[B]) => A)(implicit ka: Keyed[A], kb: Keyed[B], mb: Merger[B]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(get(a1), get(a2))))

  def merge2n[A, B, C](getB: A => List[B], getC: A => List[C], set: (A, List[B], List[C]) => A)(implicit ka: Keyed[A], kb: Keyed[B], kc: Keyed[C], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)), listMerge(getC(a1), getC(a2))))

  def merge3n[A, B, C, D](getB: A => List[B], getC: A => List[C], getD: A => List[D], set: (A, List[B], List[C], List[D]) => A)(implicit ka: Keyed[A], kb: Keyed[B], kc: Keyed[C], kd: Keyed[D], mb: Merger[B], mc: Merger[C], md: Merger[D]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)), listMerge(getC(a1), getC(a2)), listMerge(getD(a1), getD(a2))))

  def merge1n1[A, B, C](getB: A => List[B], getC: A => C, set: (A, List[B], C) => A)(implicit ka: Keyed[A], kb: Keyed[B], kc: Keyed[C], mb: Merger[B], mc: Merger[C]): Merger[A] =
    idMerger[A]((a1, a2) => set(a1, listMerge(getB(a1), getB(a2)).getOrElse(getB(a1)), mc(getC(a1), getC(a2)).getOrElse(getC(a1))))

  def listMerge[A](x: List[A], y: List[A])(implicit k: Keyed[A], merge: Merger[A]): List[A] =
    y.foldRight[List[A]](x)((a, acc) => valueMerge(acc, a))

  def valueMerge[A](x: List[A], y: A)(implicit k: Keyed[A], merge: Merger[A]): List[A] = {
    (x.foldRight((some(y), nil[A])) {
      case (v, (None, acc)) => (None, v :: acc)
      case (v, (Some(a), acc)) =>
        merge(v, a) match {
          case None => (Some(a), v :: acc)
          case Some(x) => (None, x :: acc)
        }
    }) match {
      case (value, acc) => value.fold(_ :: acc, acc)
    }
  }

}