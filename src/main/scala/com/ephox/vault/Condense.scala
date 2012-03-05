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

  implicit def CondenseShow[A: Show]: Show[Condense[A]] =
    shows(t => "condense(value = " + t.value.shows + ", c = " + t.c + ")")

  implicit def CondenseEqual[A: Equal]: Equal[Condense[A]] =
    equalBy(t => (t.value, t.c))

  implicit def CondenseOrder[A: Order]: Order[Condense[A]] =
    orderBy(t => (t.value, t.c))
}
