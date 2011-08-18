package com.ephox.vault


sealed trait Condense[A] {
  val value: A
  val c: Int
}

object Count extends Condenses

trait Condenses {
  def condense[A](v: A, ct: Int): Condense[A] = new Condense[A] {
    val value = v
    val c = ct
  }

  import collection.immutable.Map

  def condenseWithMax[A](n: Int, d: List[Condense[A]])(implicit cmp: Ordering[A]): (List[Condense[A]], Option[Int]) = {
    def minViewWithKey[K, V](m: Map[K, V])(implicit cmp: Ordering[(K, V)]): Option[(K, V, Map[K, V])] =
      if(m.isEmpty)
        None
      else {
        val (k, v) = m.min
        Some(k, v, m - k)
      }

    val (u, v) =
      d.zipWithIndex.foldRight((Map.empty[A, (Int, Int)], None: Option[Int])){
        case ((dd, x), (m, k)) => {
          val s = dd.value
          val i = dd.c
          if(m.size == n - 1 || n < 1) {
            val w = k map (i+) orElse (Some(i))
            minViewWithKey(m) match {
              case None => (m, w)
              case Some((ss, (ii, xx), mm)) =>
                if(ii < i) (mm + ((s, (i, x))), k map (ii+) orElse (Some(ii)))
                else (m, w)
            }
          } else
            (m + ((s, (i, x))), None)
        }
      }

    (u.toList sortBy (_._2._2) map {
      case (k, (v, _)) => condense(k, v)
    }, v)
  }

  import scalaz._, Scalaz._

  implicit def CondenseShow[A: Show]: Show[Condense[A]] =
    shows(t => "condense(value = " + t.value.shows + ", c = " + t.c + ")")

  implicit def CondenseEqual[A: Equal]: Equal[Condense[A]] =
    equalBy(t => (t.value, t.c))

  implicit def CondenseOrder[A: Order]: Order[Condense[A]] =
    orderBy(t => (t.value, t.c))
}
