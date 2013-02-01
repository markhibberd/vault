package able

import scalaz._, Scalaz._
import java.sql._
import scala.collection.mutable.ListBuffer

case class FromDb[A](run: (Int, ResultSet) => String \/ (A, Int)) {
  def map[B](f: A => B) =
    flatMap(a => FromDb((i, rs) => (f(a), i).right))

  def flatMap[B](f: A => FromDb[B]) =
    FromDb((i, rs) => run(i, rs).fold(_.left, {
      case (a, ii) => f(a).run(ii, rs)
    }))
}

object FromDb {
  def get[A](implicit from: FromDb[A]) =
    from

  implicit def FromDbTuple2[A: FromDb, B: FromDb]: FromDb[(A, B)] =
    for {
      a <- get[A]
      b <- get[B]
    } yield (a, b)

  implicit def FromDbTuple3[A: FromDb, B: FromDb, C: FromDb]: FromDb[(A, B, C)] =
    for {
      a <- get[A]
      b <- get[B]
      c <- get[C]
    } yield (a, b, c)


  implicit def FromDbInt: FromDb[Int] = FromDb((i, rs) =>
    try {
      (Option(rs.getInt(i)).getOrElse(-1), i + 1).right
    } catch {
      case _ => ("Not an int [" + i + "]").left
    }
  )

  implicit def FromDbString: FromDb[String] = FromDb((i, rs) =>
    try {
      (Option(rs.getString(i)).getOrElse(""), i + 1).right
    } catch {
      case _ => ("Not a string [" + i + "]").left
    }
  )

  def skip(n: Int): FromDb[Unit] = FromDb((i, rs) =>
    ((), i + n).right
  )
}

object Db {
  def get[A: FromDb](sql: String, f: (PreparedStatement => Unit) = _ => ()) =
    list[A](sql, f).map(_.headOption)

  def list[A](sql: String, f: (PreparedStatement => Unit) = _ => ())(implicit from: FromDb[A]) = {
    val connection = Connections.meta.create()
    val q = connection.prepareStatement(sql)
    f(q)
    val rs = q.executeQuery()

    try {
      var buffer = ListBuffer[A]()
      var max = 0
      while (rs.next && max < 1000) {
        max = max + 1;
        var (x, _) = from.run(1, rs).fold(_ => error("i made a mistake"), a => a)
        buffer += x
      }

      buffer.toList.right
    } finally {
      rs.close
      q.close
      connection.close
    }
  }
}
