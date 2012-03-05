package com.ephox.vault;

import scalaz._, Scalaz._, IterV._

trait VaultIteratees {
  def combineAll[A](implicit merge: Merger[A]): IterV[A, List[A]] =
     IterV.repeat[A, Option[A], List](combine) map (_.flatten)

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
