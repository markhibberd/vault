package com.ephox.vault


sealed trait Count[A] {
  val value: A
  val c: Int
}

object Count extends Counts

trait Counts {
  def count[A](v: A, ct: Int): Count[A] = new Count[A] {
    val value = v
    val c = ct
  }

  import collection.immutable.Map

  def condense[A](n: Int, d: List[Count[A]])(implicit cmp: Ordering[A]): (List[Count[A]], Option[Int]) = {
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
      case (k, (v, _)) => count(k, v)
    }, v)
  }
}
