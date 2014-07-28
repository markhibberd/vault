package vault

import scala.language.experimental.macros
import scalaz._, Scalaz._
import java.sql.Types

case class ToDb[A](private val run: (Int, Sql, Option[A]) => DbValue[Int], arity: Int) {
  def |+|[B](o: ToDb[B]): ToDb[(A, B)] =
    ToDb((n, s, ab) => ab match {
      case Some((a, b)) => run(n, s, Some(a)).fold(DbValue.fail, nn => o.run(nn, s, Some(b)))
      case None => run(n, s, None).fold(DbValue.fail, nn => o.run(nn, s, None))
    }, arity + o.arity)

  def contramap[B](f: B => A): ToDb[B] =
    ToDb((n, s, b) => run(n, s, b.map(f)), arity)

  def execute(s: Sql, a: A): DbValue[Unit] =
    run(1, s, Option(a)) map (_ => ())
}

object ToDb extends GeneratedToDb {
  def of[A: ToDb] =
    implicitly[ToDb[A]]

  def execute[A: ToDb](s: Sql, a: A): DbValue[Unit] =
    of[A].execute(s, a)

  private def run[A: ToDb](n: Int, s: Sql, a: A): DbValue[Int] =
    of[A].run(n, s, Option(a))

  private def toDb[A](run: (Int, Sql, Option[A]) => DbValue[Unit]) =
    ToDb[A]((n, s, a) => run(n, s, a).map(_ => n + 1), 1)

  private def toDbBind[A](run: (BindParam, A) => DbValue[Unit], sqlType: Int) =
    toDb[A]((n, s, a) => a match {
      case None => s.nul(n)(sqlType)
      case Some(aa) => run(s.toBind(n), aa)
    })

  implicit def ToDbOption[A: ToDb]: ToDb[Option[A]] =
    ToDb[Option[A]]((n, s, a) => of[A].run(n, s, a.flatten), of[A].arity)

  implicit def ByteToDb: ToDb[Byte] =
    toDbBind(_.byte(_), Types.SMALLINT)

  implicit def ShortToDb: ToDb[Short] =
    toDbBind(_.short(_), Types.SMALLINT)

  implicit def IntToDb: ToDb[Int] =
    toDbBind(_.int(_), Types.INTEGER)

  implicit def LongToDb: ToDb[Long] =
    toDbBind(_.long(_), Types.INTEGER)

  implicit def FloatToDb: ToDb[Float] =
    toDbBind(_.float(_), Types.FLOAT)

  implicit def DoubleToDb: ToDb[Double] =
    toDbBind(_.double(_), Types.DOUBLE)

  implicit def StringToDb: ToDb[String] =
    toDbBind(_.string(_), Types.VARCHAR)

  implicit def BooleanToDb: ToDb[Boolean] =
    toDbBind(_.boolean(_), Types.BOOLEAN)

  implicit def BigDecimalToDb: ToDb[BigDecimal] =
    toDbBind(_.bigdecimal(_), Types.NUMERIC)

  implicit def DateToDb: ToDb[java.sql.Date] =
    toDbBind((s, v) => s.date(v), Types.DATE)

  implicit def TimeToDb: ToDb[java.sql.Time] =
    toDbBind(_.time(_), Types.TIME)

  implicit def TimestampToDb: ToDb[java.sql.Timestamp] =
    toDbBind(_.timestamp(_), Types.TIMESTAMP)

  implicit def UnitToDb: ToDb[Unit] =
    ToDb[Unit]((_, _, _) => DbValue.ok(0), 0)

  import shapeless._

  def derive[A](implicit ev: ProductTypeClass[ToDb]): ToDb[A] =
    macro GenericMacros.deriveProductInstance[ToDb, A]

  object auto {
    implicit def AutoToDb[A](implicit ev: ProductTypeClass[ToDb]): ToDb[A] =
      macro GenericMacros.deriveProductInstance[ToDb, A]
  }

  implicit def ToDbTypeClass: ProductTypeClass[ToDb] =
    new ProductTypeClass[ToDb] {
      def product[H, T <: HList](h: ToDb[H], t: ToDb[T]): ToDb[H :: T] =
        (h |+| t).contramap({
          case hh :: tt => (hh, tt)
        })

      def emptyProduct: ToDb[HNil] =
        UnitToDb.contramap(_ => ())

      def project[F, G](instance: => ToDb[G], to: F => G, from: G => F): ToDb[F] =
        instance.contramap(to)
    }
}
