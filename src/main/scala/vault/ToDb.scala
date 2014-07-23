package vault

case class ToDb[A](private val run: (Int, Sql, A) => DbValue[Int]) {
  def |+|[B](o: ToDb[B]): ToDb[(A, B)] =
    ToDb((n, s, ab) => ab match{
      case (a, b) => run(n, s, a).fold(DbValue.fail, nn =>
        o.run(nn, s, b))
    })

  def contramap[B](f: B => A): ToDb[B] =
    ToDb((n, s, b) => run(n, s, f(b)))

  def execute(s: Sql, a: A): DbValue[Unit] =
    run(1, s, a) map (_ => ())
}

object ToDb extends GeneratedToDb {
  def of[A: ToDb] =
    implicitly[ToDb[A]]

  def execute[A: ToDb](s: Sql, a: A): DbValue[Unit] =
    of[A].execute(s, a)

  private def run[A: ToDb](n: Int, s: Sql, a: A): DbValue[Int] =
    of[A].run(n, s, a)

  private def toDb[A](run: (Int, Sql, A) => DbValue[Unit]) =
    ToDb[A]((n, s, a) => run(n, s, a).map(_ => n + 1))

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

}
