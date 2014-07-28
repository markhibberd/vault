package vault
object Syntax {
  implicit class QueryInterpolator(val sc: StringContext) {

    private def query[A: ToDb](a: A, n: List[Int]) = Query((sc.parts zip (n :+ 0)).map({ case (s, i) => s + (1 to i).map(_ => "?").mkString(", ") }).mkString, a)


    def q() = query((), List())

    def q[A: ToDb](a: A) = query((a), List(ToDb.of[A].arity))

    def q[A: ToDb, B: ToDb](a: A, b: B) = query((a, b), List(ToDb.of[A].arity, ToDb.of[B].arity))

    def q[A: ToDb, B: ToDb, C: ToDb](a: A, b: B, c: C) = query((a, b, c), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb](a: A, b: B, c: C, d: D) = query((a, b, c, d), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb](a: A, b: B, c: C, d: D, e: E) = query((a, b, c, d, e), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb](a: A, b: B, c: C, d: D, e: E, f: F) = query((a, b, c, d, e, f), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G) = query((a, b, c, d, e, f, g), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H) = query((a, b, c, d, e, f, g, h), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I) = query((a, b, c, d, e, f, g, h, i), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J) = query((a, b, c, d, e, f, g, h, i, j), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K) = query((a, b, c, d, e, f, g, h, i, j, k), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L) = query((a, b, c, d, e, f, g, h, i, j, k, l), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M) = query((a, b, c, d, e, f, g, h, i, j, k, l, m), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity, ToDb.of[R].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity, ToDb.of[R].arity, ToDb.of[S].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity, ToDb.of[R].arity, ToDb.of[S].arity, ToDb.of[T].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb, U: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity, ToDb.of[R].arity, ToDb.of[S].arity, ToDb.of[T].arity, ToDb.of[U].arity))

    def q[A: ToDb, B: ToDb, C: ToDb, D: ToDb, E: ToDb, F: ToDb, G: ToDb, H: ToDb, I: ToDb, J: ToDb, K: ToDb, L: ToDb, M: ToDb, N: ToDb, O: ToDb, P: ToDb, Q: ToDb, R: ToDb, S: ToDb, T: ToDb, U: ToDb, V: ToDb](a: A, b: B, c: C, d: D, e: E, f: F, g: G, h: H, i: I, j: J, k: K, l: L, m: M, n: N, o: O, p: P, q: Q, r: R, s: S, t: T, u: U, v: V) = query((a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v), List(ToDb.of[A].arity, ToDb.of[B].arity, ToDb.of[C].arity, ToDb.of[D].arity, ToDb.of[E].arity, ToDb.of[F].arity, ToDb.of[G].arity, ToDb.of[H].arity, ToDb.of[I].arity, ToDb.of[J].arity, ToDb.of[K].arity, ToDb.of[L].arity, ToDb.of[M].arity, ToDb.of[N].arity, ToDb.of[O].arity, ToDb.of[P].arity, ToDb.of[Q].arity, ToDb.of[R].arity, ToDb.of[S].arity, ToDb.of[T].arity, ToDb.of[U].arity, ToDb.of[V].arity))

  }
}
