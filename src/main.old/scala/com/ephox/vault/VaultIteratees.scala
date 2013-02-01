package com.ephox.vault;

import scalaz._, Scalaz._, iteratee._

trait VaultIteratees {
  def combineAll[A](implicit merge: Merger[A]): Iteratee[A, List[A]] =
    Iteratee.repeatBuild[A, Option[A], List](combine) map (_.flatten)

  def combine[A](implicit merge: Merger[A]): Iteratee[A, Option[A]] = {
    def combinex(acc: Option[A]): Iteratee[A, Option[A]] =
      IterateeT.peekDoneOr(acc, a1 =>
        acc.fold(
          a2 => {
            merge(a1, a2) match {
              case None => IterateeT.done(acc, Input.Empty.apply)
              case Some(a3) =>
                IterateeT.drop[A, Id](1) flatMap (_ => combinex(Some(a3)))
          }} , IterateeT.drop[A, Id](1) flatMap (_ => combinex(Some(a1)))
      ))
    combinex(None)
  }

}

object VaultIteratee extends VaultIteratees
