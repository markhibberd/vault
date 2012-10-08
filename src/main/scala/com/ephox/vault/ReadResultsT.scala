package com.ephox.vault

import sql._, SqlT._, XSqlT._
import scalaz._, Scalaz._
import scalaz.Digit._0

sealed trait ReadResultsT[F[+_], +A] {
  import ReadResultsT._

  def impl: (ResultSet, Int) => F[(Int, XSql[A])] =
    ReadResultsT.impl(this)

  def map[B](f: A => B)(implicit F: Functor[F]): ReadResultsT[F, B] =
    ReadResultsTImpl((r, n) => F.map(impl(r, n))(_ :-> (_ map f)))

  def ap[B](f: ReadResultsT[F, A => B])(implicit F: Bind[F]): ReadResultsT[F, B] =
    ReadResultsTImpl((r, n) =>
      F.bind(f.impl(r, n)) {
        case (o, y) =>
          F.map(impl(r, o))(_ :-> (_ ap y ))
      })

  def flatMap[B](f: A => ReadResultsT[F, B])(implicit F: Monad[F]): ReadResultsT[F, B] =
    ReadResultsTImpl((r, n) =>
      F.bind(impl(r, n)) {
        case (o, w) => {
          w.fold[F[(Int, XSql[B])]](F.point((o, XSqlT.empty[Id, B])), e => F.point((o, XSqlT.err[Id, B](e))), v => f(v).impl(r, o))
        }
      }
    )

  def orElse[AA >: A](x: => ReadResultsT[F, AA])(implicit F: Monad[F]): ReadResultsT[F, AA] =
    ReadResultsTImpl((r, n) =>
      F.bind(impl(r, n)) {
        case (o, s) => s.fold[F[(Int, XSql[AA])]](F.point((o, s)), _ => F.point((o, s)), _ => x.impl(r, n))
      })

  def |||[AA >: A](x: => ReadResultsT[F, AA])(implicit F: Monad[F]): ReadResultsT[F, AA] =
    orElse(x)

  def product[B](x: ReadResultsT[F, B])(implicit F: Bind[F]): ReadResultsT[F, (A, B)] =
    x ap map(a => (a, _))

  def ***[B](x: ReadResultsT[F, B])(implicit F: Bind[F]): ReadResultsT[F, (A, B)] =
    product(x)

  def apply(r: ResultSet)(implicit F: Functor[F]): XSqlT[F, A] =
    XSqlT(F.map(impl(r, 1))(_._2.run))

  def @@>(r: ResultSet)(implicit F: Functor[F]): OptionT[F, A] =
    apply(r).toOption

  def @>(r: ResultSet)(implicit F: Functor[F]): OptionT[F, SqlError \/ A] =
    apply(r).transformer

  def skip(x: Int)(implicit F: Functor[F]): ReadResultsT[F, A] =
    if(x <= 0)
      this
    else
      ReadResultsTImpl((r, n) => F.map(impl(r, n)) {
        case (t, z) => (t + x, z)
      })

  def ++(x: Int)(implicit F: Functor[F]): ReadResultsT[F, A] =
    skip(x)

}
private case class ReadResultsTImpl[F[+_], +A](read: (ResultSet, Int) => F[(Int, XSql[A])]) extends ReadResultsT[F, A]

object ReadResultsT {
  private def impl[F[+_], A](r: ReadResultsT[F, A]): (ResultSet, Int) => F[(Int, XSql[A])] =
    r match {
      case ReadResultsTImpl(i) => i
    }

  type ReadResults[+A] =
    ReadResultsT[Id, A]

  private def read[A](f: (ResultSet, Int) => XSql[A]): ReadResults[A] =
    ReadResultsTImpl[Id, A]((r, i) => {
      val q = f(r, i)
      q.fold(
        (i + 1, XSqlT.empty[Id, A])
      , e => (i, XSqlT.err[Id, A](e))
      , a => (i + 1, ~r.wasNull flatMap (z => if(z) XSqlT.empty else XSqlT.value(a))))
    })

  implicit def UnitReadResults: ReadResults[Unit] =
    ReadResultsTImpl[Id, Unit]((_, i) => (i, XSqlT.value(())))

  implicit val ArrayReadResults: ReadResults[Array] =
    read((r, i) => r.array(Column(i)).get)

  implicit val AsciiStreamReadResults: ReadResults[java.io.InputStream] =
    read((r, i) => r.asciiStream(Column(i)).get)

  implicit val BigDecimalReadResults: ReadResults[java.math.BigDecimal] =
    read((r, i) => r.bigDecimal(Column(i)).get)

  implicit val BinaryStreamReadResults: ReadResults[java.io.InputStream] =
    read((r, i) => r.binaryStream(Column(i)).get)

  implicit val BlobReadResults: ReadResults[Blob] =
    read((r, i) => r.blob(Column(i)).get)

  implicit val BooleanReadResults: ReadResults[Boolean] =
    read((r, i) => ~r.boolean(Column(i)).get)

  implicit val ByteReadResults: ReadResults[Byte] =
    read((r, i) => ~r.byte(Column(i)).get)

  implicit val ScalaArrayReadResults: ReadResults[scala.Array[Byte]] =
    read((r, i) => r.bytes(Column(i)).get)

  implicit val CharacterStreamReadResults: ReadResults[java.io.Reader] =
    read((r, i) => r.characterStream(Column(i)).get)

  implicit val ClobReadResults: ReadResults[Clob] =
    read((r, i) => r.clob(Column(i)).get)

  def readDate(m: Option[java.util.Calendar]): ReadResults[Date] =
    read((r, i) => r.date(Column(i), m).get)

  implicit val DateReadResults: ReadResults[Date] =
    readDate(None)

  implicit val DoubleReadResults: ReadResults[Double] =
    read((r, i) => ~r.double(Column(i)).get)

  implicit val FloatReadResults: ReadResults[Float] =
    read((r, i) => ~r.float(Column(i)).get)

  implicit val IntReadResults: ReadResults[Int] =
    read((r, i) => ~r.int(Column(i)).get)

  implicit val LongReadResults: ReadResults[Long] =
    read((r, i) => ~r.long(Column(i)).get)

  def readObject(m: Option[collection.mutable.Map[String, Class[_]]]): ReadResults[AnyRef] =
    read((r, i) => r.obj(Column(i), m).get)

  implicit val ObjectReadResults: ReadResults[AnyRef] =
    readObject(None)

  implicit val RefReadResults: ReadResults[Ref] =
    read((r, i) => r.ref(Column(i)).get)

  implicit val ShortReadResults: ReadResults[Short] =
    read((r, i) => ~r.short(Column(i)).get)

  implicit val StringReadResults: ReadResults[String] =
    read((r, i) => r.string(Column(i)).get)

  def readTime(m: Option[java.util.Calendar]): ReadResults[Time] =
    read((r, i) => r.time(Column(i), m).get)

  implicit val TimeReadResults: ReadResults[Time] =
    readTime(None)

  def readTimestamp(m: Option[java.util.Calendar]): ReadResults[Timestamp] =
    read((r, i) => r.timestamp(Column(i), m).get)

  implicit val TimestampReadResults: ReadResults[Timestamp] =
    readTimestamp(None)

  implicit val URLReadResults: ReadResults[java.net.URL] =
    read((r, i) => r.url(Column(i)))

  implicit def OptionReadResults[A](implicit v: ReadResults[A]): ReadResults[Option[A]] =
    ReadResultsTImpl[Id, Option[A]]((r, n) =>
      v.impl(r, n) :-> (r => (r.map(Some(_)) ?? XSqlT.value(None))))

  implicit def Tuple2ReadResults[A, B](implicit va: ReadResults[A], vb: ReadResults[B]): ReadResults[(A, B)] =
    for {
      a <- va
      b <- vb
    } yield (a, b)

  implicit def Tuple3ReadResults[A, B, C](implicit va: ReadResults[A], vb: ReadResults[B], vc: ReadResults[C]): ReadResults[(A, B, C)] =
    for {
      a <- va
      b <- vb
      c <- vc
    } yield (a, b, c)

  implicit def Tuple4ReadResults[A, B, C, D](implicit va: ReadResults[A], vb: ReadResults[B], vc: ReadResults[C], vd: ReadResults[D]): ReadResults[(A, B, C, D)] =
    for {
      a <- va
      b <- vb
      c <- vc
      d <- vd
    } yield (a, b, c, d)

  implicit def Tuple5ReadResults[A, B, C, D, E](implicit va: ReadResults[A], vb: ReadResults[B], vc: ReadResults[C], vd: ReadResults[D], ve: ReadResults[E]): ReadResults[(A, B, C, D, E)] =
    for {
      a <- va
      b <- vb
      c <- vc
      d <- vd
      e <- ve
    } yield (a, b, c, d, e)

  implicit def Tuple6ReadResults[A, B, C, D, E, F](implicit va: ReadResults[A], vb: ReadResults[B], vc: ReadResults[C], vd: ReadResults[D], ve: ReadResults[E], vf: ReadResults[F]): ReadResults[(A, B, C, D, E, F)] =
    for {
      a <- va
      b <- vb
      c <- vc
      d <- vd
      e <- ve
      f <- vf
    } yield (a, b, c, d, e, f)

  implicit def Tuple7ReadResults[A, B, C, D, E, F, G](implicit va: ReadResults[A], vb: ReadResults[B], vc: ReadResults[C], vd: ReadResults[D], ve: ReadResults[E], vf: ReadResults[F], vg: ReadResults[G]): ReadResults[(A, B, C, D, E, F, G)] =
    for {
      a <- va
      b <- vb
      c <- vc
      d <- vd
      e <- ve
      f <- vf
      g <- vg
    } yield (a, b, c, d, e, f, g)
}
