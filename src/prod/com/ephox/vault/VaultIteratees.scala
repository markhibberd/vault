package com.ephox.vault;

import scalaz._, Scalaz._, IterV._

trait VaultIteratees {
  def combineAll[A](implicit merge: Merger[A]): IterV[A, List[A]] = {
    def step(current: Option[A], acc: List[A])(s: Input[A]): IterV[A, List[A]] =
        s(el = a2 => current match {
          case None => Cont(step(Some(a2), acc))
          case Some(a1) => merge(a1, a2) match {
            case None =>
              Cont(step(Some(a2), a1 :: acc))
            case Some(a3) =>
              Cont(step(Some(a3), acc))
         }
        },
        empty = Cont(step(current, acc)),
        eof = Done(current match {
          case None => acc.reverse
          case Some(a) => (a :: acc).reverse
        }, EOF.apply))
    Cont(step(None, Nil))
  }

  def combine[A](implicit merge: Merger[A]): IterV[A, Option[A]] = {
    def combinex(acc: Option[A]): IterV[A, Option[A]] =
      IterV.peekDoneOr(acc, a1 =>
        acc.fold(
          a2 => {
            merge(a1, a2) match {
              case None => IterV.Done(acc, IterV.Empty.apply)
              case Some(a3) => IterV.drop(1) >>=| combinex(Some(a3))
          }} , IterV.drop(1) >>=| combinex(Some(a1))
      ))
    combinex(None)
  }

}

object VaultIteratee extends VaultIteratees
