package com.ephox.vault


sealed trait Condense[A] {
  val value: A
  val c: Int
}

object Condense extends Condenses

trait Condenses {
  def condense[A](v: A, ct: Int): Condense[A] = new Condense[A] {
    val value = v
    val c = ct
  }

  def condenseWithMax[A](n: Int, d: List[Condense[A]])(implicit cmp: Ordering[A]): (List[Condense[A]], Option[Int]) = {
    val (x, y) = d.zipWithIndex sortWith(_._1.c > _._1.c) splitAt (n - 1)

    (x sortWith (_._2 < _._2) map (_._1), y match {
      case Nil    => None
      case (g::t) => Some(t.foldLeft(g._1.c)(_ + _._1.c))
    })
  }

  import scalaz._, Scalaz._

  trait CondenseEqual[A] extends Equal[Condense[A]] {
    implicit def A: Equal[A]

    override def equalIsNatural: Boolean = A.equalIsNatural

    override def equal(a1: Condense[A], a2: Condense[A]) =
      a1.value == a2.value && a1.c === a2.c
  }

  trait CondenseOrder[A] extends Order[Condense[A]] {
    implicit def A: Order[A]

    override def order(a1: Condense[A], a2: Condense[A]) =
      Order.orderBy((t: Condense[A]) => (t.value, t.c)) apply (a1, a2)
  }

  implicit def CondenseShow[A: Show]: Show[Condense[A]] = new Show[Condense[A]] {
    def show(t: Condense[A]) =
      ("condense(value = " + t.value.shows + ", c = " + t.c + ")").toList
  }

  implicit def CondenseOrder[A](implicit A0: Order[A]): Order[Condense[A]] = new CondenseOrder[A] {
    implicit def A = A0
  }
}
