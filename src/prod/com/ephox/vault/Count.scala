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

/*
condense ::
  Ord a =>
  Int
  -> [Data a]
  -> ([Data a], Maybe Int)
condense n d =
  let u = foldr (\(Datum s i, x) (m, k) ->
            if M.size m == n - 1 || n < 1
              then let w = fmap (+ i) k `mplus` Just i
                   in case M.minViewWithKey m of
                             Nothing             -> (m, w)
                             Just ((s', (i', x')), m') -> if i' < i
                                                            then (M.insert s (i, x) m', fmap (+ i') k `mplus` Just i')
                                                            else (m, w)
              else (M.insert s (i, x) m, Nothing)) (M.empty, Nothing) (d `zip` [0..])
  in first (fmap (uncurry $ \k -> Datum k . fst) . sortBy (comparing (snd . snd)) . M.toList) u

test =
  [
    "Bar"  `Datum` 2
  , "Fred" `Datum` 7
  , "Baz"  `Datum` 4
  , "Bob"  `Datum` 5
  ]

*/