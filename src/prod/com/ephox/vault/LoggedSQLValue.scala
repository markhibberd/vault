package com.ephox.vault

import scalaz._
import Scalaz._

import LoggedSQLValue._

sealed trait LoggedSQLValue[A] extends NewType[WriterT[SQLValue, LOG, A]] {
  val value: WriterT[SQLValue, LOG, A]
}

object LoggedSQLValue {
  type LOG = List[String] // todo use better data structure

  implicit def LoggedSQLValuePure: Pure[LoggedSQLValue] = new Pure[LoggedSQLValue] {
    def pure[A](a: => A) = new LoggedSQLValue[A] {
      val value = a.η[({type λ[α]= WriterT[SQLValue, LOG, α]})#λ]
    }
  }

  implicit def LoggedSQLValuerFunctor: Functor[LoggedSQLValue] = new Functor[LoggedSQLValue] {
    def fmap[A, B](k: LoggedSQLValue[A], f: A => B) = new LoggedSQLValue[B] {
      val value = {
        val ftr = WriterT.WriterTFunctor[SQLValue, LOG]
        // todo why won't the implicit pick up?
        ftr.fmap(k.value, f)
      }
    }
  }

}

trait LoggedSQLValues {
  def loggedSqlValue[A](w: WriterT[SQLValue, LOG, A]): LoggedSQLValue[A] = new LoggedSQLValue[A] {
    val value = w
  }
}