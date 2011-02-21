package com.ephox.vault

import scalaz._
import Scalaz._

sealed trait WriterT[M[_], W, A] {
  val value: M[(W, A)]
}

object WriterT {
  def writerT[M[_], W, A](v: M[(W, A)]): WriterT[M, W, A] = new WriterT[M, W, A] {
    val value = v
  }

  implicit def WriterTPure[M[_]: Pure, W: Zero]: Pure[({type λ[α]= WriterT[M, W, α]})#λ] = new Pure[({type λ[α]=WriterT[M, W, α]})#λ] {
    def pure[A](a: => A) = writerT((∅[W], a).η[M])
  }

  implicit def WriterTFunctor[M[_]: Functor, W]: Functor[({type λ[α]=WriterT[M, W, α]})#λ] = new Functor[({type λ[α]= WriterT[M, W, α]})#λ] {
    def fmap[A, B](x: WriterT[M, W, A], f: A => B) = writerT(x.value ∘ (_ ∘ f))
  }

  implicit def WriterTApply[M[_], W](implicit ma: Apply[M], ftr: Functor[M], ss: Semigroup[W]): Apply[({type λ[α]=WriterT[M, W, α]})#λ] = new Apply[({type λ[α]=WriterT[M, W, α]})#λ] {
    def apply[A, B](f: WriterT[M, W, A => B], a: WriterT[M, W, A]): WriterT[M, W, B] =
      writerT[M, W, B](f.value.<**>(a.value) { case ((w1, fff), (w2, aaa)) => (w1 |+| w2, fff(aaa)) })

  }

  implicit def WriterTBind[M[_], W](implicit mnd: Monad[M], sg: Semigroup[W]): Bind[({type λ[α]=WriterT[M, W, α]})#λ] = new Bind[({type λ[α]=WriterT[M, W, α]})#λ] {
    def bind[A, B](a: WriterT[M, W, A], f: A => WriterT[M, W, B]) = writerT(
      a.value >>= {
        case (w1, aaa) => {
          val hh = f(aaa).value
          hh ∘ { case (w2, b) => (w1 |+| w2, b) }
        }
      })
  }

  implicit def WriterTEach[M[_]: Each, W]: Each[({type λ[α]=WriterT[M, W, α]})#λ] = new Each[({type λ[α]= WriterT[M, W, α]})#λ] {
    def each[A](x: WriterT[M, W, A], f: A => Unit) = x.value foreach { case (_, a) => f(a) }
  }
}
