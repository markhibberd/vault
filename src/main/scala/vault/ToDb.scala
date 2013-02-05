package vault

case class ToDb[A](private val run: (Int, Sql, A) => DbValue[Int]) {
  def |+|[B](o: ToDb[B]): ToDb[(A, B)] =
    ToDb((n, s, ab) => ab match{
      case (a, b) => run(n, s, a).fold(DbValue.fail, inc =>
        o.run(n, s, b))
    })

  def comap[B](f: B => A): ToDb[B] =
    ToDb((n, s, b) => run(n, s, f(b)))

  def execute(s: Sql, a: A): DbValue[Unit] =
    run(1, s, a) map (_ => ())
}

object ToDb {
  def set[A: ToDb] =
    implicitly[ToDb[A]]

  private def toDb[A](run: (Int, Sql, A) => DbValue[Unit]) =
    ToDb[A]((n, s, a) => run(n, s, a).map(_ => 1))

  private def toDbBind[A](run: (BindParam, A) => DbValue[Unit]) =
    toDb[A]((n, s, a) => run(s.toBind(n), a))

  implicit def ToDbUnit: ToDb[Unit] =
    ToDb[Unit]((_, _, _) => DbValue.ok(0))

  implicit def ToDbInt: ToDb[Int] =
    toDbBind(_.int(_))

  implicit def ToDbLong: ToDb[Long] =
    toDbBind(_.long(_))

  implicit def ToDbString: ToDb[String] =
    toDbBind(_.string(_))

  implicit def ToDbBoolean: ToDb[Boolean] =
    toDbBind(_.boolean(_))

  implicit def ToDbTuple2[A: ToDb, B: ToDb]: ToDb[(A, B)] =
    set[A] |+| set[B]

  implicit def ToDbTuple3[A: ToDb, B: ToDb, C: ToDb]: ToDb[(A, B, C)] =
    (set[A] |+| set[B] |+| set[C]).comap({
      case (a, b, c) => ((a, b), c)
    })

  implicit def ToDbTuple4[A: ToDb, B: ToDb, C: ToDb, D: ToDb]: ToDb[(A, B, C, D)] =
    (set[A] |+| set[B] |+| set[C] |+| set[D]).comap({
      case (a, b, c, d) => (((a, b), c), d)
    })
}
