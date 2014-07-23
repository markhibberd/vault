package vault

trait GeneratedFromDb { this: FromDb.type  =>
  implicit def FromDbTuple2[A: FromDb, B: FromDb]: FromDb[(A, B)] = for {
    a <- of[A]
    b <- of[B]
  } yield (a, b)

  implicit def FromDbTuple3[A: FromDb, B: FromDb, C: FromDb]: FromDb[(A, B, C)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
  } yield (a, b, c)

  implicit def FromDbTuple4[A: FromDb, B: FromDb, C: FromDb, D: FromDb]: FromDb[(A, B, C, D)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
  } yield (a, b, c, d)

  implicit def FromDbTuple5[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb]: FromDb[(A, B, C, D, E)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
  } yield (a, b, c, d, e)

  implicit def FromDbTuple6[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb]: FromDb[(A, B, C, D, E, F)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
  } yield (a, b, c, d, e, f)

  implicit def FromDbTuple7[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb]: FromDb[(A, B, C, D, E, F, G)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
  } yield (a, b, c, d, e, f, g)

  implicit def FromDbTuple8[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb]: FromDb[(A, B, C, D, E, F, G, H)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
  } yield (a, b, c, d, e, f, g, h)

  implicit def FromDbTuple9[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
  } yield (a, b, c, d, e, f, g, h, i)

  implicit def FromDbTuple10[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
  } yield (a, b, c, d, e, f, g, h, i, j)

  implicit def FromDbTuple11[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
  } yield (a, b, c, d, e, f, g, h, i, j, k)

  implicit def FromDbTuple12[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l)

  implicit def FromDbTuple13[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m)

  implicit def FromDbTuple14[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n)

  implicit def FromDbTuple15[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o)

  implicit def FromDbTuple16[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p)

  implicit def FromDbTuple17[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q)

  implicit def FromDbTuple18[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb, R: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
    r <- of[R]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r)

  implicit def FromDbTuple19[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb, R: FromDb, S: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
    r <- of[R]
    s <- of[S]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s)

  implicit def FromDbTuple20[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb, R: FromDb, S: FromDb, T: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
    r <- of[R]
    s <- of[S]
    t <- of[T]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t)

  implicit def FromDbTuple21[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb, R: FromDb, S: FromDb, T: FromDb, U: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
    r <- of[R]
    s <- of[S]
    t <- of[T]
    u <- of[U]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u)

  implicit def FromDbTuple22[A: FromDb, B: FromDb, C: FromDb, D: FromDb, E: FromDb, F: FromDb, G: FromDb, H: FromDb, I: FromDb, J: FromDb, K: FromDb, L: FromDb, M: FromDb, N: FromDb, O: FromDb, P: FromDb, Q: FromDb, R: FromDb, S: FromDb, T: FromDb, U: FromDb, V: FromDb]: FromDb[(A, B, C, D, E, F, G, H, I, J, K, L, M, N, O, P, Q, R, S, T, U, V)] = for {
    a <- of[A]
    b <- of[B]
    c <- of[C]
    d <- of[D]
    e <- of[E]
    f <- of[F]
    g <- of[G]
    h <- of[H]
    i <- of[I]
    j <- of[J]
    k <- of[K]
    l <- of[L]
    m <- of[M]
    n <- of[N]
    o <- of[O]
    p <- of[P]
    q <- of[Q]
    r <- of[R]
    s <- of[S]
    t <- of[T]
    u <- of[U]
    v <- of[V]
  } yield (a, b, c, d, e, f, g, h, i, j, k, l, m, n, o, p, q, r, s, t, u, v)

}
