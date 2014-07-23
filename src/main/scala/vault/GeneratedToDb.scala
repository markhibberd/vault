package vault

trait GeneratedToDb { this: ToDb.type  =>
  implicit def ToDbTuple2[A: ToDb, B: ToDb]: ToDb[(A, B)] =
    (of[A] |+| of[B]).contramap({
      case (a, b) => (a, b)
    })

  implicit def ToDbTuple3[A: ToDb, B: ToDb, C: ToDb]: ToDb[(A, B, C)] =
    (of[A] |+| of[B] |+| of[C]).contramap({
      case (a, b, c) => ((a, b), c)
    })

  implicit def ToDbTuple4[A: ToDb, B: ToDb, C: ToDb, D: ToDb]: ToDb[(A, B, C, D)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D]).contramap({
      case (a, b, c, d) => (((a, b), c), d)
    })

  implicit def ToDbTuple5[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb]: ToDb[(A, B, C, D, E)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E]).contramap({
      case (a, b, c, d, e) => ((((a, b), c), d), e)
    })

  implicit def ToDbTuple6[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb]: ToDb[(A, B, C, D, E, F)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F]).contramap({
      case (a, b, c, d, e, f) => (((((a, b), c), d), e), f)
    })

  implicit def ToDbTuple7[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb]: ToDb[(A, B, C, D, E, F, G)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G]).contramap({
      case (a, b, c, d, e, f, g) => ((((((a, b), c), d), e), f), g)
    })

  implicit def ToDbTuple8[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb]: ToDb[(A, B, C, D, E, F, G, H)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H]).contramap({
      case (a, b, c, d, e, f, g, h) => (((((((a, b), c), d), e), f), g), h)
    })

  implicit def ToDbTuple9[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I]).contramap({
      case (a, b, c, d, e, f, g, h, i) => ((((((((a, b), c), d), e), f), g), h), i)
    })

  implicit def ToDbTuple10[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J]).contramap({
      case (a, b, c, d, e, f, g, h, i, j) => (((((((((a, b), c), d), e), f), g), h), i), j)
    })

  implicit def ToDbTuple11[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k) => ((((((((((a, b), c), d), e), f), g), h), i), j), k)
    })

  implicit def ToDbTuple12[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l) => (((((((((((a, b), c), d), e), f), g), h), i), j), k), l)
    })

  implicit def ToDbTuple13[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m) => ((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m)
    })

  implicit def ToDbTuple14[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n) => (((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n)
    })

  implicit def ToDbTuple15[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o) => ((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o)
    })

  implicit def ToDbTuple16[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p) => (((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p)
    })

  implicit def ToDbTuple17[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q) => ((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q)
    })

  implicit def ToDbTuple18[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q] |+| of[R]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r) => (((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r)
    })

  implicit def ToDbTuple19[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q] |+| of[R] |+| of[S]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s) => ((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s)
    })

  implicit def ToDbTuple20[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q] |+| of[R] |+| of[S] |+| of[T]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t) => (((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t)
    })

  implicit def ToDbTuple21[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb, U: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q] |+| of[R] |+| of[S] |+| of[T] |+| of[U]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u) => ((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u)
    })

  implicit def ToDbTuple22[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb, U: ToDb, V: ToDb]: ToDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] =
    (of[A] |+| of[B] |+| of[C] |+| of[D] |+| of[E] |+| of[F] |+| of[G] |+| of[H] |+| of[I] |+| of[J] |+| of[K] |+| of[L] |+| of[M] |+| of[N] |+| of[O] |+| of[P] |+| of[Q] |+| of[R] |+| of[S] |+| of[T] |+| of[U] |+| of[V]).contramap({
      case (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v) => (((((((((((((((((((((a, b), c), d), e), f), g), h), i), j), k), l), m), n), o), p), q), r), s), t), u), v)
    })

}
